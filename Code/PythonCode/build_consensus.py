import pandas as pd
import os
import re


def top_n_adjlists(adjlist_scores, n=10):
    '''Read in the top n scoring adjlists.
    Inputs
    ------
    adjlist_scores: pandas DataFrame with columns for score and fname
    dict where keys are scores and values are file names
    n: number of top adjlists to keep. Defaults to 10.

    Returns
    -------
    List of length n, where each element is an adjacency list. Each
    adjacency list is stored as a pandas DataFrame.
    '''
    adjlist_scores = adjlist_scores.sort_values(
        by='score', ascending=False)
    adjlists = list()
    # reduce n if it is larger than the total number of adjlists
    n = min(n, adjlist_scores.shape[0])
    for i in range(n):
        fname = adjlist_scores['fname'].iloc[i]
        adjlist = pd.read_csv(fname)
        adjlists.append(adjlist)
    return adjlists


def get_adjlist_scores(path):
    '''Extract adjlist scores from the titles of files.
    Inputs
    ------
    path: str, path to folder containing adjlists

    Returns
    -------
    pandas DataFrame with columns for score and fname
    dict where keys are scores and values are file names
    '''

    fs = os.listdir(path)
    scores = list()
    fnames = list()
    for f in fs:
        if re.match('.*adjlist.csv$', f):
            score = f.split('-')[-2]
            scores.append(float(score))
            fnames.append(os.path.join(path, f))
    # Build pandas df
    adjlist_scores = pd.DataFrame(data={'score': scores,
                                        'fname': fnames})
    return adjlist_scores


def add_adjlist_to_dict(adjlist, full_adjlist_dict=None):
    '''Add the links from an adjlist to the full dict of links.
    Inputs
    ------
    adjlist: pandas DataFrame, adjacency list with "Child" and "Parent"
    columns.
    full_adjlist_dict: dict where keys are a [Child, Parent] list and values
    are the number of times that link has been found in a top adjlist. If
    full_adjlist_dict is set to `None`, it will be initialized in this
    function.

    Returns
    -------
    full_adjlist_dict, updated with links from adjlist.
    '''
    if full_adjlist_dict is None:
        full_adjlist_dict = dict()

    for i in range(adjlist.shape[0]):
        key = (adjlist['Child'].iloc[i], adjlist['Parent'].iloc[i])
        if key in full_adjlist_dict.keys():
            full_adjlist_dict[key] += 1
        else:
            full_adjlist_dict[key] = 1
    return full_adjlist_dict


def consensus_from_full_adjlist(full_adjlist_dict, num_adjlists, cutoff=.5):
    '''Create a consensus adjlist based on the support for different links
    in full_adjlist_dict.
    Inputs
    ------
    full_adjlist_dict: dict where keys are a [Child, Parent] list and values
    are the number of times that link has been found in a top adjlist.
    num_adjlists: int, number of adjacency lists used to build
                  full_adjlist_dict
    cutoff: percentage of adjacency lists that must have the link present for
    it to be included in consensus_adjlist. Defaults to .5.

    Returns
    -------
    consensus_df: pandas DataFrame, consensus adjacency list with "Child",
    "Parent", and "Consensus_Percent" columns.
    '''
    # intialize adjlist with header
    headers = ["Child", "Parent", "Consensus_Percent"]
    consensus_adjlist = []
    for key, value in full_adjlist_dict.items():
        # check if the support for this link is past the cutoff
        support = float(value) / float(num_adjlists)
        # if it's good enough, add it to the consensus adjlist
        # along with the percent support for the link
        if support >= cutoff:
            consensus_adjlist.append([key[0], key[1], support])
    consensus_df = pd.DataFrame(consensus_adjlist, columns=headers)
    return consensus_df


def compare_consensus_best(best_adjlist, consensus_adjlist):
    '''Compare the best adjlist to the consensus adjlist, and report
    number of differing links.
    Inputs
    ------
    best_adjlist: pandas DataFrame, the best-scoring adjacency list, containing
    columns "Child" and "Parent.
    consensus_adjlist: pandas DataFrame, consensus adjacency list with "Child",
    "Parent", and "Consensus_Percent" columns.

    Returns
    -------
    dict, with three key-value pairs:
        'not_in_best': number of links in consensus_adjlist not in best_adjlist
        'not_in_consensus': number of links in best_adjlist not in
                            consensus_adjlist
        'pct_diff': (not_in_best + not_in_consensus)/
                    (links in best_adjlist + links in consensus_adjlist)
    '''
    best_set = set(tuple(row) for row in best_adjlist.values)
    consensus_set = set(tuple(row) for row in
                        consensus_adjlist[['Child', 'Parent']].values)
    not_in_best = len(consensus_set - best_set)
    not_in_consensus = len(best_set - consensus_set)
    pct_diff = (not_in_best + not_in_consensus) / \
        (len(best_set) + len(consensus_set))
    out = {'not_in_best': not_in_best,
           'not_in_consensus': not_in_consensus,
           'pct_diff': pct_diff}
    return out


def build_Comte_consensus(path, outfile, n=10, cutoff=.5):
    '''Build a consensus network.

    Inputs
    ------
    path: str, path to folder containing adjlists
    outfile: str, path to csv file where consensus adjlist will be written
    n: number of top adjlists to keep. Defaults to 10.
    cutoff: percentage of adjacency lists that must have the link present for
    it to be included in consensus_adjlist. Defaults to .5.
    '''

    adjlist_scores = get_adjlist_scores(path)
    adjlists = top_n_adjlists(adjlist_scores, n=n)
    full_adjlist_dict = None
    for adjlist in adjlists:
        full_adjlist_dict = add_adjlist_to_dict(
            adjlist,
            full_adjlist_dict=full_adjlist_dict)
    consensus = consensus_from_full_adjlist(full_adjlist_dict,
                                            len(adjlists),
                                            cutoff=cutoff)
    # TODO: report similarity to best network?
    if outfile is not None:
        consensus.to_csv(outfile, index=False)
    return [consensus, adjlists]


def build_Tatoosh_consensus(inpath, outpath, n=10, cutoff=.5):
    '''Build a consensus network for each LOO cross-validation set for
    the Tatoosh system.

    Inputs
    ------
    inpath: str, path to folder containing folders with xml files for different
    cross validation sets
    outpath: str, path to folder containing folders for adding consensus files
    for different cross validation sets
    n: number of top adjlists to keep. Defaults to 10.
    cutoff: percentage of adjacency lists that must have the link present for
    it to be included in consensus_adjlist. Defaults to .5.
    '''

    for root, subdirs, files in os.walk(inpath):
        for x in subdirs:
            # All folders besides Marginals contain LOO cross validation data
            if re.match('.*[0-9]+-[0-9]+$', os.path.basename(x)):
                outfile = os.path.join(outpath,
                                       os.path.basename(x),
                                       os.path.basename(x) + '-consensus.csv')
                build_Comte_consensus(os.path.join(inpath, x), outfile)
