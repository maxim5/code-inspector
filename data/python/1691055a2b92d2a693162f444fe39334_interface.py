"""
interface to programatically provide custom ticket fields
"""

from trac.core import Interface

class ITicketSidebarProvider(Interface):
    
    def enabled(req, ticket):
        """returns whether this plugin should be rendered for this ticket"""

    def content(req, ticket):
        """content to be rendered in the sidebar"""
