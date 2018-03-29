"""
interface to programatically provide custom ticket fields
"""

from trac.core import Interface

class ICustomFieldProvider(Interface):
    
    def fields():
        """
        should return a dictionary of dictionaries describing the custom
        fields.  The primary key should be the field name.  The secondary keys
        should be the options and the secondary values should be field values

        Example:
        { 'mycustomfield': { 'type': 'radio',
                             'label': 'My Custom Field',
                             'options': ['foo', 'bar', 'baz']} }

        'type' == 'text' is assumed if not provided

        See http://trac.edgewall.org/wiki/TracTicketsCustomFields
        for defined semantics
        """
