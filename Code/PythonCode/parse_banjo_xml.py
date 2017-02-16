import re
import untangle
import numpy
import os
import sys


def build_spname_dict(spfname):
    '''
    Build a dictionary of species names and the number used
    to represent them in banjo.

    Inputs
    ------
    spfname: string, path to file of species names

    Returns
    -------
    spdict_abbrev: dict, keys are the integer representation of species as given
        in the banjo xml file, values are species names
    '''
    spdict_abbrev = dict()
    i = 0
    with open(spfname, 'r') as f:
        for line in f:
            if i >= 0:
                line = line.rstrip().split(',')
                # convert from numbers to short names
                spdict_abbrev[str(i)] = line[0]
            i += 1
    return spdict_abbrev


def structures_from_xml(fname):
    '''
    Read file and build list of xml networks

    Inputs
    ------
    fname: string, path to xml file name

    Returns
    -------
    list of Python untangle objects, representing the networks in the
    fname file
    '''
    try:
        xml_data = untangle.parse(fname)
        xml_nets = xml_data.BanjoData.nBestNetworks.network
        return xml_nets
    except:
        print("Failed to read xml file.")
        raise


def structure_to_dict(xml_net):
    '''
    Build a dictionary of the adjacency structure from Python untangle object.

    Inputs
    ------
    xml_net: untangle object, data from banjo xml file

    Returns
    -------
    dict, keys are Child species, values are lists of Parent species
    '''
    # get the structure from the xml as a big unicode string
    net_structure = xml_net.networkStructure.cdata
    net_structure = net_structure.split('\n')
    net_dict = dict()
    for line in net_structure:
        line = re.split('\s+1:\s+[0-9]+\s', line)
        # if it's a data line, there will be two pieces to line now
        if len(line) == 2:
            # strip remaining whitespace
            line = [item.split() for item in line]
            if len(line[0]) == 1:
                net_dict[line[0][0]] = line[1]
            else:
                raise ValueError
    return net_dict


def structure_to_score(xml_net):
    '''
    Extract structure score from Python untangle object.

    Inputs
    ------
    xml_net: untangle object, data from banjo xml file

    Returns
    -------
    string, network score
    '''
    net_score = xml_net.networkScore.cdata
    net_score = re.split('\n', net_score)
    return net_score[1]


def dict_to_adjlist(net_dict, spdict_abbrev):
    '''
    Convert a dictionary of interactions to a list of lists

    Inputs
    ------
    net_dict: dict, keys are Child species, values are lists of Parent species
    spdict_abbrev: dict, keys are the integer representation of species as given
        in the banjo xml file, values are species names

    Returns
    -------
    net_adjlist: list of lists, first element is the header row ['Child', 'Parent'],
        and each later element of the list is a [child, parent] pair
    '''
    net_adjlist = [['Child', 'Parent']]
    for child, parents in net_dict.items():
        try:
            child_spname = spdict_abbrev[child]
            for parent in parents:
                parent_spname = spdict_abbrev[parent]
                net_adjlist.append([child_spname, parent_spname])
        except:
            raise ValueError
    return net_adjlist


def build_net_fname(fname, net_score):
    '''
    Build output file name for a structure with a given score

    Inputs
    ------
    fname: string, path to xml file name
    net_score: string, score of a network structure

    Returns
    -------
    net_fname: string, file name where structure will be written
    '''
    net_fname = fname.rsplit('-', 1)[0]
    net_fname = net_fname + '-score' + net_score + '-adjlist.csv'
    return net_fname


def parse_banjo_xml(fname, spdict_abbrev):
    '''
    Parse xml output from banjo, for a single file.

    Inputs
    ------
    fname: string, path to xml file name
    spdict_abbrev: dict, keys are the integer representation of species as given
        in the banjo xml file, values are species names

    Returns
    -------
    float, best structure score from the xml file
    '''
    try:
        xml_nets = structures_from_xml(fname)
    except:
        print('ERROR: ' + fname)
        return None
    scores = []
    for xml_net in xml_nets:
        net_dict = structure_to_dict(xml_net)
        net_score = structure_to_score(xml_net)
        scores.append(float(net_score))
        net_adjlist = dict_to_adjlist(net_dict, spdict_abbrev)
        net_fname = build_net_fname(fname, net_score)
        net_adjlist = numpy.asarray(net_adjlist)
        numpy.savetxt(net_fname, net_adjlist, fmt='%s', delimiter=',')
    return max(scores)


def parse_all_xml(path, spfname, delete=False):
    '''
    Parse xml output from banjo, for an entire folder of files.

    Inputs
    ------
    path: string, path to folder containing xml files
    spfname: string, path to file containing species names
    delete: bool, flags if xml files should be deleted

    Returns
    -------
    all_scores: list, list of the best scores for each xml file
    '''

    spdict_abbrev = build_spname_dict(spfname)
    fs = os.listdir(path)
    all_scores = []
    for f in fs:
        if re.match('.*xml$', f):
            print(f)
            all_scores.append(parse_banjo_xml(os.path.join(path, f),
                                              spdict_abbrev))
            if delete:
                os.system('rm %s' % (os.path.join(path, f)))
    return all_scores

# if __name__ == "__main__":
# path = sys.argv[1]  # path to xml files
# spfname = sys.argv[2]  # file containing species names
# delete = int(sys.argv[3])  # flag for deleting xml files

#     for root, subdirs, files in os.walk(path):
#         for x in subdirs:
#             fullpath = os.path.join(path, x)
#             all_scores = parse_all_xml(fullpath, spfname, delete)
#             all_scores.sort(reverse=True)
#             os.system('rm ' + fullpath + '/*.sh')
#             os.system('rm ' + fullpath + '/*tmp.txt')
