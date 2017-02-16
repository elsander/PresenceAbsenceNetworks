import os
import unittest
import parse_banjo_xml as parse


class xmlTest(unittest.TestCase):

    def tearDown(self):
        os.system('rm banjo_test.xml-score-25919.7635-adjlist.csv')
        os.system('rm banjo_test.xml-score-25919.8905-adjlist.csv')
        os.system('rm banjo_test.xml-score-25919.1071-adjlist.csv')
        os.system('rm banjo_test.xml-score-25919.3371-adjlist.csv')
        os.system('rm banjo_test.xml-score-25919.3406-adjlist.csv')

    def test_build_spname_dict(self):
        spf = '../../Data/Comte-data/Comte-Species-final.txt'
        spdict_abbrev = parse.build_spname_dict(spf)
        assert len(spdict_abbrev) == 31
        assert spdict_abbrev['0'] == 'Ala'
        assert spdict_abbrev['1'] == 'Ana'
        assert spdict_abbrev['30'] == 'Lel'

    def test_structures_from_xml(self):
        f = 'banjo_test.xml'
        xml_nets = parse.structures_from_xml(f)
        assert len(xml_nets) == 5
        assert xml_nets[0].networkScore.cdata == '\n-25919.7635\n'

    def test_structure_to_dict(self):
        f = 'banjo_test.xml'
        xml_nets = parse.structures_from_xml(f)
        net_dict = parse.structure_to_dict(xml_nets[0])
        assert net_dict['0'] == ['0', '2']
        assert net_dict['1'] == ['1']
        assert net_dict['2'] == ['2']
        assert net_dict['3'] == ['3']
        assert net_dict['4'] == ['4', '5']
        assert net_dict['5'] == ['0', '5']

    def test_structure_to_score(self):
        f = 'banjo_test.xml'
        xml_nets = parse.structures_from_xml(f)
        net_score = parse.structure_to_score(xml_nets[0])
        assert net_score == '-25919.7635'

    def test_dict_to_adjlist(self):
        spf = '../../Data/Comte-data/Comte-Species-final.txt'
        spdict_abbrev = parse.build_spname_dict(spf)
        f = 'banjo_test.xml'
        xml_nets = parse.structures_from_xml(f)
        net_dict = parse.structure_to_dict(xml_nets[0])
        net_adjlist = parse.dict_to_adjlist(net_dict, spdict_abbrev)
        assert ['Ala', 'Ala'] in net_adjlist
        assert ['Ala', 'Bam'] in net_adjlist
        assert ['Ana', 'Ana'] in net_adjlist
        assert ['Bam', 'Bam'] in net_adjlist
        assert ['Tes', 'Tes'] in net_adjlist
        assert ['Blb', 'Blb'] in net_adjlist
        assert ['Blb', 'Abb'] in net_adjlist
        assert ['Abb', 'Ala'] in net_adjlist
        assert ['Abb', 'Abb'] in net_adjlist

    def test_build_net_fname(self):
        f = 'banjo_test-1-1.xml'
        score = '-9435.1242'
        expected_f = 'banjo_test-1-score-9435.1242-adjlist.csv'
        out_f = parse.build_net_fname(f, score)
        assert out_f == expected_f

    def test_parse_banjo_xml(self):
        f = 'banjo_test.xml'
        spf = '../../Data/Comte-data/Comte-Species-final.txt'
        spdict_abbrev = parse.build_spname_dict(spf)
        score = parse.parse_banjo_xml(f, spdict_abbrev)
        self.assertAlmostEqual(score, -25919.7635)
