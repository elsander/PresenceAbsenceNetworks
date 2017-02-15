import re
import untangle
import numpy
import os
import sys
import ipdb


def build_spname_dict_Comte(spfname):
    spdict_full = dict()
    spdict_abbrev = dict()
    # counting from -1 so that we skip the header
    i = -1
    with open(spfname, 'r') as f:
        for line in f:
            if i >= 0:
                line = line.rstrip().split(',')
                # convert from numbers to short names
                spdict_abbrev[str(i)] = line[1]
                # convert from short names to full (for graphing)
                spdict_full[line[1]] = line[0]
            i += 1
    return spdict_full, spdict_abbrev


def build_spname_dict_Tatoosh(spfname, metadata=False):
    spdict_abbrev = dict()
    with open(spfname, 'r') as f:
        if metadata:
            # count from -5 to skip metadata column names
            i = -5
        else:
            # count from 0 if using file without metadata columns
            i = 0
        for line in f:
            if i >= 0:
                spdict_abbrev[str(i)] = line.strip('\n')
            i += 1
    return spdict_abbrev


def structures_from_xml(fname):
    try:
        xml_data = untangle.parse(fname)
        xml_nets = xml_data.BanjoData.nBestNetworks.network
        return xml_nets
    except:
        # need to raise an appropriate error here, or try something else
        raise ValueError


def structure_to_dict(xml_net):
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
    net_score = xml_net.networkScore.cdata
    net_score = re.split('\n', net_score)
    return net_score[1]


def dict_to_adjlist(net_dict, spdict_abbrev):
    net_adjlist = [['Child', 'Parent']]
    for child, parents in net_dict.items():
        try:
            child_spname = spdict_abbrev[child]
            for parent in parents:
                parent_spname = spdict_abbrev[parent]
                net_adjlist.append([child_spname, parent_spname])
        except:
            ipdb.set_trace()
    return net_adjlist


def build_net_fname(fname, net_score):
    net_fname = fname.rsplit('-', 1)[0]
    # net_fname = re.split('\.xml', fname)[0]
    net_fname = net_fname + '-score' + net_score
    return net_fname


def graphviz_plot(net_adjlist, spdict_full, Comte=True):
    # build graphviz dot file
    graphviz_string = ' digraph G {\n'
    for pair in net_adjlist:
        if pair[1] == 'Parent':
            continue
        if Comte:
            graphviz_string += '%s -> %s;\n' % (spdict_full[pair[1]],
                                                spdict_full[pair[0]])
        else:
            graphviz_string += '%s -> %s;\n' % (pair[1], pair[0])
    graphviz_string += '}'
    return graphviz_string


def parse_banjo_xml(fname, spdict_full, spdict_abbrev, graphviz=True,
                    Comte=True):
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
        if graphviz:
            graphviz_string = graphviz_plot(
                net_adjlist, spdict_full, Comte=Comte)
            with open(net_fname + '.dot', 'w') as g:
                g.write(graphviz_string)
            # compile into svg
            os.system('dot -Tsvg %s.dot -o %s.svg' % (net_fname, net_fname))
        net_adjlist = numpy.asarray(net_adjlist)
        numpy.savetxt(net_fname + '-adjlist.csv',
                      net_adjlist, fmt='%s', delimiter=',')
    return max(scores)


def parse_all_xml(path, spfname, graphviz=True, delete=False, Comte=True):
    if Comte:
        spdict_full, spdict_abbrev = build_spname_dict_Comte(spfname)
    else:
        spdict_abbrev = build_spname_dict_Tatoosh(spfname)
        spdict_full = None
    fs = os.listdir(path)
    all_scores = []
    for f in fs:
        if re.match('.*xml$', f):
            print(f)
            all_scores.append(parse_banjo_xml(os.path.join(path, f),
                                              spdict_full,
                                              spdict_abbrev,
                                              graphviz=graphviz,
                                              Comte=Comte))
            if delete:
                os.system('rm %s' % (os.path.join(path, f)))
    return all_scores

if __name__ == "__main__":
    path = sys.argv[1] # path to xml files
    spfname = sys.argv[2] # file containing species names
    graphviz = int(sys.argv[3]) # flag for creating graphviz graphs
    delete = int(sys.argv[4]) # flag for deleting xml files
    Comte = int(sys.argv[5]) # flag for Comte (1) or Tatoosh (0)

    for root, subdirs, files in os.walk(path):
        for x in subdirs:
            fullpath = os.path.join(path, x)
            all_scores = parse_all_xml(fullpath, spfname, graphviz, delete, Comte=Comte)
            all_scores.sort(reverse=True)
            os.system('rm ' + fullpath + '/*.sh')
            os.system('rm ' + fullpath + '/*tmp.txt')
