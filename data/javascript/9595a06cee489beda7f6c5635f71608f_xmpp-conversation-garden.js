/**
 * Manage a number of conversations
 */

var Conversation = require('./xmpp-conversation.js').Conversation

/**
 * @param cl
 *   The XMPP client
 */
function ConversationGarden(cl) {

  var conversations = {}
  var conversationObservers = []

  /**
   * Create a conversation with this contact, if it doesn't exist.
   *
   * @param string contact
   *   The name (email) of the contact
   */
  function conversationWith(contact) {
    if (!conversations[contact]) {
      var conv = new Conversation(cl, contact)
      for (var i = 0; i < conversationObservers.length; ++i) {
        conversationObservers[i].addConversation(conv, contact)
      }
      conversations[contact] = conv
    }
    return conversations[contact]
  }

  /**
   * Callback for stanza events on the client.
   * We check if that is a message with a body. If it is, we lazy-create the
   * conversation for this contact, and let it handle the message.
   *
   * @param stanza
   *   The stanza element
   */
  function onStanza(stanza) {
    if (stanza.is('message')) {
      if (stanza.attrs.type != 'error') {
        var text = ''
        stanza.getChildren('body').forEach(function(bodyElement){
          text += bodyElement.children.join('')
        })
        if (text.length) {
          // TODO: Why is the address sitting in 'to' instead of 'from' ?
          var contact = stanza.attrs.from.split('/')[0]
          conversationWith(contact).messageFromContact(text)
        }
      }
    }
    else if (stanza.is('presence')) {
      var contact = stanza.attrs.from.split('/')[0]
      // Just create the conversation. Do nothing more.
      conversationWith(contact)
    }
  }

  // register the onStanza event handler.
  cl.on('stanza', onStanza)

  /**
   * Subscribes a conversation observer, that will be notified about all
   * existing conversations, and every new conversation.
   *
   * The observer then has the chance to register a bot to the conversation.
   * Yes, most "observers" are bot managers.
   *
   * @param observer
   *   Observer to be notified of conversations
   */
  this.observeConversations = function(observer) {
    for (var contact in conversations) {
      observer.addConversation(conversations[contact], contact)
    }
    conversationObservers.push(observer)
  }
}

exports.ConversationGarden = ConversationGarden
