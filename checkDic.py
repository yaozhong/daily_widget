#coding=UTF8
# -*- encoding: utf-8 -*-
"""
Japanese character combination search
"""

import DictionaryServices as D
import itertools as it

def char_permutations(combinationsinput):
	comb = it.permutations(input)
	combList = ["".join(w) for w in list(comb)]
	return combList

##############################################
# input the potential characters in the list
##############################################
input = [u'ひ', u'こ', u'う', u'き']
#input = [u'し', u'う', u'', u'']

for w in  char_permutations(input):
	# checking dictionary
	word_range = (0, len(input))
	word_definition = D.DCSCopyTextDefinition(None, w, word_range)
	if word_definition != None:
		print w

