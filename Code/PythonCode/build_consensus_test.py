import pandas as pd
import os
import unittest
import numpy.testing
import build_consensus as bc


class bcTest(unittest.TestCase):

    def adjlist_file_setup(self):
        path = 'bc_test'
        os.mkdir(path)
        test_1 = {'Child': ['a', 'b', 'b', 'c'],
                  'Parent': ['a', 'b', 'c', 'c']}
        pd_1 = pd.DataFrame(test_1)
        pd_1.to_csv('bc_test/test_1-10000-adjlist.csv', index=False)
        test_2 = {'Child': ['a', 'b', 'b', 'c', 'c'],
                  'Parent': ['a', 'b', 'c', 'c', 'a']}
        pd_2 = pd.DataFrame(test_2)
        pd_2.to_csv('bc_test/test_2-500-adjlist.csv', index=False)
        return path, pd_1, pd_2

    def adjlist_file_teardown(self):
        os.remove('bc_test/test_1-10000-adjlist.csv')
        os.remove('bc_test/test_2-500-adjlist.csv')
        if os.path.isfile('bc_test/out.tmp'):
            os.remove('bc_test/out.tmp')
        os.rmdir('bc_test')

    def test_top_n_adjlists(self):
        __, pd_1, pd_2 = self.adjlist_file_setup()
        adjlist_scores = pd.DataFrame(
            {'score': [10000, 500],
             'fname': ['bc_test/test_1-10000-adjlist.csv',
                       'bc_test/test_2-500-adjlist.csv']})
        adjlists = bc.top_n_adjlists(adjlist_scores)
        self.assertEqual(len(adjlists), 2)
        assert pd_1.equals(adjlists[0])
        assert pd_2.equals(adjlists[1])
        adjlists = bc.top_n_adjlists(adjlist_scores, n=1)
        self.assertEqual(len(adjlists), 1)
        assert pd_1.equals(adjlists[0])
        self.adjlist_file_teardown()

    def test_get_adjlist_scores(self):
        path, __, __ = self.adjlist_file_setup()
        adjlist_scores = bc.get_adjlist_scores(path)
        fnames = set(['bc_test/test_1-10000-adjlist.csv',
                      'bc_test/test_2-500-adjlist.csv'])
        scores = set([10000, 500])
        assert fnames == set(adjlist_scores.fname)
        assert scores == set(adjlist_scores.score)
        self.adjlist_file_teardown()

    def test_add_adjlist_to_dict(self):
        __, pd_1, pd_2 = self.adjlist_file_setup()
        fulldict = bc.add_adjlist_to_dict(pd_1)
        assert max(fulldict.values()) == 1
        assert len(fulldict) == 4
        assert fulldict[('b', 'c')] == 1
        fulldict = bc.add_adjlist_to_dict(pd_2, full_adjlist_dict=fulldict)
        assert max(fulldict.values()) == 2
        assert len(fulldict) == 5
        assert fulldict[('b', 'c')] == 2
        assert fulldict[('c', 'a')] == 1
        self.adjlist_file_teardown()

    def test_consensus_from_full_adjlist(self):
        fulldict = dict()
        fulldict[('a', 'a')] = 4
        fulldict[('a', 'b')] = 2
        fulldict[('b', 'b')] = 1
        consensus = bc.consensus_from_full_adjlist(fulldict, 4, cutoff=.5)
        self.assertEqual(consensus.shape, (2, 3))
        support = sorted(list(consensus['Consensus_Percent'].values))
        consensus = set(tuple(row) for row in
                        consensus[['Child', 'Parent']].values)

        # add tests that specific rows are present
        assert ('a', 'a') in consensus
        assert ('a', 'b') in consensus
        numpy.testing.assert_almost_equal([.5, 1], support)
        consensus = bc.consensus_from_full_adjlist(fulldict, 4, cutoff=.75)
        self.assertEqual(consensus.shape, (1, 3))

    def test_compare_consensus_best(self):
        __, pd_1, pd_2 = self.adjlist_file_setup()
        out = bc.compare_consensus_best(pd_1, pd_2)
        self.assertEqual(out['not_in_best'], 1)
        self.assertEqual(out['not_in_consensus'], 0)
        numpy.testing.assert_almost_equal(out['pct_diff'], 1 / 9)
        self.adjlist_file_teardown()

    def test_build_Comte_consensus(self):
        __, pd_1, pd_2 = self.adjlist_file_setup()
        outfile = 'bc_test/out.tmp'
        [consensus, adjlists] = bc.build_Comte_consensus('bc_test',
                                                         outfile,
                                                         n=10,
                                                         cutoff=.6)
        consensus = set([tuple(row) for row in consensus[['Child', 'Parent']]])
        set_adj_1 = set([tuple(row) for row in adjlists[0]])
        set_adj_2 = set([tuple(row) for row in adjlists[1]])
        set_pd_1 = set([tuple(row) for row in pd_1])
        set_pd_2 = set([tuple(row) for row in pd_2])
        assert os.path.isfile(outfile)
        self.assertEqual(len(adjlists), 2)
        assert consensus == set_adj_1
        assert set_adj_1 == set_pd_1
        assert set_adj_2 == set_pd_2
        self.adjlist_file_teardown()
