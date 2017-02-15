import parse_banjo_xml as parse

def parse_banjo_xml_tests():
    ## NOTE: tests currently only have coverage
    ## for Comte data
    spf = '../../Data/Comte2015/SpeciesCodes.csv'
    spdict_full, spdict_abbrev = parse.build_spname_dict_Comte(spf)
    assert len(spdict_full) == 32
    assert len(spdict_abbrev) == 32
    assert spdict_abbrev['0'] == 'Ala'
    assert spdict_abbrev['1'] == 'Ana'
    assert spdict_abbrev['31'] == 'Lel'
    assert spdict_full['Ala'] == 'Alburnus_alburnus'
    assert spdict_full['Ana'] == 'Anguilla_anguilla'
    assert spdict_full['Lel'] == 'Leuciscus_leuciscus'

    f = 'test.xml'
    xml_nets = parse.structures_from_xml(f)
    assert len(xml_nets) == 5

    net_dict = parse.structure_to_dict(xml_nets[0])
    assert net_dict['0'] == ['0', '2'] 
    assert net_dict['1'] == ['1']
    assert net_dict['2'] == ['2']
    assert net_dict['3'] == ['3']
    assert net_dict['4'] == ['4', '5']
    assert net_dict['5'] == ['0', '5']

    net_score = parse.structure_to_score(xml_nets[0])
    assert net_score == '-25919.7635'

    net_adjlist = parse.dict_to_adjlist(net_dict, spdict_abbrev)
    assert ['Ala', 'Ala'] in net_adjlist
    assert ['Ala', 'Bab'] in net_adjlist
    assert ['Ana', 'Ana'] in net_adjlist
    assert ['Bab', 'Bab'] in net_adjlist
    assert ['Bam', 'Bam'] in net_adjlist
    assert ['Tes', 'Tes'] in net_adjlist
    assert ['Tes', 'Blb'] in net_adjlist
    assert ['Blb', 'Ala'] in net_adjlist
    assert ['Blb', 'Blb'] in net_adjlist
