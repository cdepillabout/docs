#!/usr/bin/env python
"""
This code downloads lists from scanki and converts them 
into tab-delimited files. It tries to get all media.
"""
import sys
import traceback
import urllib2
try:
    import argparse
except:
    print "ERROR! Could not import the argparse library."
    print "This program needs the argparse library."

try:
    from lxml import etree
except:
    print "ERROR! Could not import etree from the lxml library."
    print "This program needs the lxml library."

VOCAB_NAMESPACE = ''
DEBUG = False
VERSION = "0.1"

# This is a hack.  You can put IPSHELL() in places you want to do debugging.
# It will just exit in normal mode, and open an IPython Shell 
# when you're in debug mode.
def __raise():
    sys.exit(1)
IPSHELL = __raise

def debug_node(node):
    """ Print out all the debugging info about a node I could ever want."""
    ret_string = ''

    ret_string += "Node: " + str(node) + "\n"
    ret_string += "Attributes: " + str(node.attrib) + "\n"
    ret_string += "Base: " + str(node.base) + "\n"
    ret_string += "Index from parent: " + str(node.getparent().index(node)) + "\n"
    ret_string += "Tag: " + str(node.tag) + "\n"
    ret_string += "Text: " + str(node.text) + "\n"
    ret_string += "Ancestors:\n"
    for n in node.iterancestors():
        ret_string += "\t" + str(n) + ": " + str(n.attrib) + "\n"
    ret_string += "Children:\n"
    for n in node.iterchildren():
        ret_string += "\t" + str(n) + ": " + str(n.attrib) + "\n"

    return ret_string

def print_stack():
    """This just prints a stack trace like python would do."""
    stack = traceback.extract_stack()
    print >> sys.stderr, "Traceback (most recent call last):"
    for frame in traceback.format_list(stack)[:-1]:
        print >> sys.stderr,  frame,

def check_japanese_attr(node):
    """Raise exception if this node does not have a language="ja" attribute."""
    # Here's one way to do it:
    #langs = xpath(node, "attribute::language")

    # Here's another way to do it:
    #for attribute in item.items():
    #    if attribute[0] == 'language':
    #        if attribute[1] != 'ja':
    #            print "Not a Japanese item: " + str(item.items())
    #            raise Exception

    # Here's probably the best way to do it:
    if xpath_one_node(node, "@language") != 'ja':
        print >> sys.stderr, "node: " + str(node) + " does not have a 'ja' language attribute."
        print >> sys.stderr, debug_node(node)
        print_stack()
        IPSHELL()

def xpath(node, xpath_expression):
    """Do xpath expression with a default 'vocab' namespace."""
    return node.xpath(xpath_expression, namespaces={'vocab':VOCAB_NAMESPACE})

def xpath_one_node(node, xpath_expression, must_exist=True, warn_if_no=False):
    """Do xpath expression on node, we are only expecting one node on result. 
    Error if more than one node.  If must_exist is True, then we error if there
    is no result. Return None if must_exist is not true and there is no result.
    If warn_if_no is set, then it returns a warning if it does not exist or 
    if there is nothing there."""
    node_list = xpath(node, xpath_expression)

    # we will error out if must_exist is set, but there is no node
    if must_exist == True and (node_list == [] or len(node_list) != 1):
        print >> sys.stderr, "xpath expression " + xpath_expression + " on node " + \
                str(node) + " was not sucessful."
        print >> sys.stderr, debug_node(node)
        print_stack()
        IPSHELL()

    # if must_exist is False and there is no node, then we can just
    # return None.  This is okay.
    if must_exist == False and (len(node_list) == 0):
        if warn_if_no:
            return "WARNING, NOTHING FOUND!"
        else:
            return ""

    # Otherwise, return the node we found.
    return node_list[0]

def process_transliterations(transliterations_node):
    hira_transliteration = xpath_one_node(transliterations_node, 
            "vocab:transliteration[@type='Hira']/text()", must_exist=False)
    hrkt_transliteration = xpath_one_node(transliterations_node, 
            "vocab:transliteration[@type='Hrkt']/text()")

    # We will have a problem if hrkt transliteration is emtpy,
    # or if hira transliteration is not empty, but it is not equal
    # to hrkt transliteration.
    if hrkt_transliteration == "":
        print >> sys.stderr, "Error! hrkt_transliteration is " + \
                hrkt_transliteration
        print >> sys.stderr, "hira_transliteration is " + hira_transliteration
        print >> sys.stderr, "Something is not right."
        print >> sys.stderr, debug_node(transliterations_node)
        print_stack()
        IPSHELL()

    return hrkt_transliteration

def process_cue(cue_node):
    check_japanese_attr(cue_node)

    # it is unfortunate that there is both a <text> tag, and a TEXT part of xml.
    # This makes the below code kind of confusing.
    # Also, will part_of_speech ever be empty??
    part_of_speech = xpath_one_node(cue_node, "@part_of_speech")
    text = xpath_one_node(cue_node, "vocab:text/text()")
    character = xpath_one_node(cue_node, "vocab:character/text()", must_exist=False)
    sound = xpath_one_node(cue_node, "vocab:sound/text()", must_exist=False)
    transliteration = process_transliterations(
            xpath_one_node(cue_node, "vocab:transliterations"))

    # make sure the text node is equal to the transliteration node
    if transliteration != text:
        print >> sys.stderr, "Error! Transliteration (" + transliteration +\
                ") does not equal text (" + text + ")"
        print >> sys.stderr, debug_node(node)
        print_stack()
        IPSHELL()

    return {'character': character.encode('utf8'),
            'text': text.encode('utf8'),
            'part_of_speech': part_of_speech,
            'sound': sound,
            'transliteration': transliteration.encode('utf8')}

def process_response(responses_node):
    english_meaning = xpath_one_node(responses_node, 
            "vocab:response[@type='meaning'][@language='en']/vocab:text/text()")
    """
    print "\tresponses: "
    print "\t\tresponse: " + english_meaning 
    """
    return english_meaning 

def process_sentence(sentence_node):
    check_japanese_attr(sentence_node)

    sentence = xpath_one_node(sentence_node, "vocab:text/text()")
    image = xpath_one_node(sentence_node, "vocab:image/text()", must_exist=False) 
    sound = xpath_one_node(sentence_node, "vocab:sound/text()", must_exist=False)
    transliteration = process_transliterations(
            xpath_one_node(sentence_node, "vocab:transliterations"))
    translation = xpath_one_node(sentence_node, 
            "./vocab:translations/vocab:sentence[@language='en']/vocab:text/text()",
            warn_if_no=True, must_exist=False)

    """
    print "\t\tsentence:"
    print "\t\t\tsentence: " + sentence.encode('utf-8')
    print "\t\t\timage: " + str(image)
    print "\t\t\tsound: " + str(sound) 
    print "\t\t\ttransliteration: " + transliteration.encode('utf-8')
    print "\t\t\ttranslation: " + translation
    """

    return {'sentence': sentence.encode('utf8'),
            'image': str(image),
            'sound': str(sound),
            'transliteration': transliteration.encode('utf8'),
            'translation': translation}

def process_sentences(sentences_node):
    sentences = xpath(sentences_node, "./vocab:sentence")

    if len(sentences) < 1:
        print >> sys.stderr, "Error! No sentences!"
        print >> sys.stderr, debug_node(node)
        print_stack()
        IPSHELL()

    sentence_list = []
    for sentence in sentences:
        sentence_list.append(process_sentence(sentence))

    return sentence_list

def process_item(item_node):
    """ Processes one item.  This should make two flash cards, 
    assuming there are two sentences."""
    check_japanese_attr(item_node)

    cue = process_cue(xpath_one_node(item_node, "vocab:cue"))
    response = process_response(xpath_one_node(item_node, "vocab:responses"))
    sentence_list = process_sentences(xpath_one_node(item_node, "vocab:sentences"))

    return {'cue': cue,
            'response': response,
            'sentences': sentence_list}


def print_item(item):
    """Prints a item made with process_item().
    The format of the resulting line will be the following,
    separated by tabs (if something doesn't exist, the space
    will still be left for it):
        
    # this is just for the word
    word (kanji if it exists, otherwise just kana)
    kanji (if it exists)
    kana
    part of speech (noun, verb, etc)
    sound (url of sound)
    transliteration (should be same as kana)
    english meaning
    # the rest will be multiple sentences
    # with this layout
    sentence in kanji
    image url
    sound url
    transliteration (sentence in kana)
    english translation
    """

    # word will become the kanji if it exists, or just the word if not
    word = ""
    if len(item['cue']['character']) == 0:
        word = item['cue']['text'].encode('utf8')
    else:
        word = item['cue']['character']

    # the first parts are from cue
    """
    line = word + '\t' + \
            item['cue']['character'].encode('utf8') + '\t' + \
            item['cue']['text'].encode('utf8') + '\t' + \
            item['cue']['part_of_speech'] + '\t' + \
            item['cue']['sound'] + '\t' + \
            item['cue']['transliteration'].encode('utf8') + '\t' + \
            item['response'] + '\t'

    for sentence in item['sentences']:
        line += sentence['sentence'].encode('utf8') + '\t' + \
                sentence['image'] + '\t' + \
                sentence['sound'] + '\t' + \
                sentence['transliteration'].encode('utf8') + '\t' + \
                sentence['translation']
    """
    line = word + '\t' + \
            item['cue']['character'] + '\t' + \
            item['cue']['text'] + '\t' + \
            item['cue']['part_of_speech'] + '\t' + \
            item['cue']['sound'] + '\t' + \
            item['cue']['transliteration'] + '\t' + \
            item['response'] + '\t'

    for sentence in item['sentences']:
        line += sentence['sentence'] + '\t' + \
                sentence['image'] + '\t' + \
                sentence['sound'] + '\t' + \
                sentence['transliteration'] + '\t' + \
                sentence['translation']

    print line
    print

            




def main():
    global VOCAB_NAMESPACE
    global DEBUG
    global IPSHELL

    description = "Make a tab-delimited file containing info from a smart.fm list."
    argparser = argparse.ArgumentParser(description=description)
    argparser.add_argument('-d', '--debug', dest='debug', action='store_true',
            default=False, help='turn on debugging (needs ipython)')
    argparser.add_argument('--version', action='version', 
            version=('%(prog)s-' + VERSION), help='print version')
    args = argparser.parse_args()

    DEBUG = args.debug
    if DEBUG:
        from IPython.Shell import IPShellEmbed
        IPSHELL = IPShellEmbed([])

    #vocab_list = 19053  # core2k
    #vocab_list = 24532   # core6k
    vocab_list = 71776   # ko2001 
    url = "http://api.smart.fm/lists/" + str(vocab_list) + "/items.xml?per_page=20"

    tree = etree.parse(url)
    root = tree.getroot()

    VOCAB_NAMESPACE = root.nsmap[None]
    items = xpath(tree, "/vocab:vocabulary/vocab:items/vocab:item") 

    lala = None
    for item in items:
        item_result = process_item(item)
        get_media(item_result)
        print_item(item_result)


if __name__ == '__main__':
    main()

