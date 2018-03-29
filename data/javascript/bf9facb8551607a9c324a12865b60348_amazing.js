Zenoss.nav.appendTo('Component', [{
    id: 'amazing_stuff_bottom_panel',
    text: _t('Amazing Stuff'),
    xtype: 'componentpanel',
    action: function(node, target, combo) {
        var uid = combo.contextUid;
        alert(uid);
    }        }]);

/* Next line simply pops up a message on the screen
alert('just added amazing stuff');
*/


