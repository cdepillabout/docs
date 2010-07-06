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
    if get_attribute(node, 'language') != 'ja':
        print >> sys.stderr, "node: " + str(node) + " does not have a 'ja' language attribute."
        print >> sys.stderr, debug_node(node)
        print_stack()
        IPSHELL()

def xpath(node, xpath_expression):
    """Do xpath expression with a default 'vocab' namespace."""
    return node.xpath(xpath_expression, namespaces={'vocab':VOCAB_NAMESPACE})

def xpath_one_node(node, xpath_expression, must_exist=True):
    """Do xpath expression on node, we are only expecting one node on result. 
    Error if more than one node.  If must_exist is True, then we error if there
    is no result."""
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
        return None

    # Otherwise, return the node we found.
    return node_list[0]

def get_attribute(node, attribute):
    """Get attribute from a node.  We assume that the attribute exists.
    Error if we can't find it."""
    if (not node.attrib.has_key(attribute)):
        print >> sys.stderr, "node: " + str(node) + " does not have attribute \"" + \
                attribute + "\"."
        print >> sys.stderr, debug_node(node)
        print_stack()
        IPSHELL()
    return node.attrib[attribute]

def get_text(node, must_contain_text=True):
    """Get text from node. If must_contain_text is true, then we error out
    if it doesn't contain any text."""
    if must_contain_text and not node.text:
        print >> sys.stderr, "No text for node: " + str(node)
        print >> sys.stderr, debug_node(node)
        print_stack()
        IPSHELL()
    return node.text

def get_text_xpath(node, xpath, must_exist=True, must_contain_text=True):
    """Get text inside node matched by xpath.  If must_exist is True, then 
    we error out if this does not exist.  If must_contain_text is True, then
    we error out if this does not contain text."""
    xpath_node = xpath_one_node(node, xpath, must_exist)
    if xpath_node is not None:
        return get_text(xpath_node, must_contain_text)
    else:
        return ""

def process_transliterations(transliterations_node):

    hira_transliteration = get_text_xpath(transliterations_node, 
            "vocab:transliteration[@type='Hira']", must_exist=False)
    hrkt_transliteration = get_text_xpath(transliterations_node, 
            "vocab:transliteration[@type='Hrkt']")

    # We will have a problem if hrkt transliteration is emtpy,
    # or if hira transliteration is not empty, but it is not equal
    # to hrkt transliteration.
    if hrkt_transliteration == "" or \
            (hira_transliteration != "" and \
            hira_transliteration != hrkt_transliteration):
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
    part_of_speech = get_attribute(cue_node, "part_of_speech")
    text = get_text_xpath(cue_node, "vocab:text")
    character = get_text_xpath(cue_node, "vocab:character", must_exist=False)
    sound = get_text_xpath(cue_node, "vocab:sound", must_exist=False,
            must_contain_text=False)
    transliteration = process_transliterations(
            xpath_one_node(cue_node, "vocab:transliterations"))

    # make sure the text node is equal to the transliteration node
    if transliteration != text:
        print >> sys.stderr, "Error! Transliteration (" + transliteration +\
                ") does not equal text (" + text + ")"
        print >> sys.stderr, debug_node(node)
        print_stack()
        IPSHELL()


    print "cue: " + character + "\t" + text + "\t" + part_of_speech + \
            "\t" + sound + "\t" + transliteration

def process_response(responses_node):
    english_meaning = get_text_xpath(responses_node, 
            "vocab:response[@type='meaning'][@language='en']/vocab:text")
    print "responses: " + english_meaning 

def process_sentence(sentence_node):
    check_japanese_attr(sentence_node)

    sentence = get_text_xpath(sentence_node, "vocab:text")
    image = get_text_xpath(sentence_node, "vocab:image", must_exist=False, 
            must_contain_text=False)
    sound = get_text_xpath(sentence_node, "vocab:sound", must_exist=False,
            must_contain_text=False)
    transliteration = process_transliterations(
            xpath_one_node(sentence_node, "vocab:transliterations"))
    translation = get_text_xpath(sentence_node, 
            "vocab:translations/vocab:sentence[@language='en']/vocab:text")

    print "sentence: " + unicode(sentence) + "\t" + str(image) + "\t" + \
            str(sound) + "\t" + unicode(transliteration) + "\t" + translation


def process_sentences(sentences_node):
    sentences = xpath(sentences_node, "vocab:sentence")

    if len(sentences) < 1:
        print >> sys.stderr, "Error! No sentences!"
        print >> sys.stderr, debug_node(node)
        print_stack()
        IPSHELL()

    for sentence in sentences:
        process_sentence(sentence)

def process_item(item_node):
    """ Processes one item.  This should make two flash cards, 
    assuming there are two sentences."""
    check_japanese_attr(item_node)

    process_cue(xpath_one_node(item_node, "vocab:cue"))
    process_response(xpath_one_node(item_node, "vocab:responses"))
    process_sentences(xpath_one_node(item_node, "vocab:sentences"))

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

    list = 19053 
    url = "http://api.smart.fm/lists/" + str(list) + "/items.xml?per_page=20"

    tree = etree.parse(url)
    root = tree.getroot()

    #print etree.tostring(root, pretty_print=True, method='xml', encoding="UTF-8")
    #print etree.tostring(tree, pretty_print=True, method='text', encoding="UTF-8")

    VOCAB_NAMESPACE = root.nsmap[None]
    items = xpath(tree, "/vocab:vocabulary/vocab:items/vocab:item") 

    for item in items:
        process_item(item)


if __name__ == '__main__':
    main()

