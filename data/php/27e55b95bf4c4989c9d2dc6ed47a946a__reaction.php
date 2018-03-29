<script type="text/javascript">
var myReactions=[
  { 
    ID: 'fresh',
    iconImgUp: 'http://cdn.gigya.com/gs/i/reactions/icons/Fresh_Icon_Up.png',
    tooltip: 'This is fresh',
    feedMessage: 'Fresh!',
    headerText: 'You think this is fresh '
  },
  { 
    ID: 'worn',
    iconImgUp: 'http://cdn.gigya.com/gs/i/reactions/icons/Worn_Icon_Up.png',
    tooltip: 'This is worn',
    feedMessage: 'Worn!',
    headerText: 'You think this is worn '
  },
  { 
    ID: 'amazing',
    iconImgUp: 'http://cdn.gigya.com/gs/i/reactions/icons/Amazing_Icon_Up.png',
    tooltip: 'This is amazing',
    feedMessage: 'This is amazing!',
    headerText: 'You think this is amazing '
  },
  { 
    ID: 'weird',
    iconImgUp: 'http://cdn.gigya.com/gs/i/reactions/icons/JustWeird_Icon_Up.png',
    tooltip: 'This is just weird',
    feedMessage: 'Just weird',
    headerText: 'You think this is just weird '
  }
  ]

var act = new gigya.services.socialize.UserAction();  
act.setLinkBack('<?php echo $sf_request->getUri();?>');
act.setTitle('This is the post title');

var showReactionsBarUI_params=
{ 
  barID: 'barID',
  showCounts: 'right',
  containerID: 'reactionDiv',
  reactions: myReactions,
  userAction: act,
  showSuccessMessage: true,
  noButtonBorders: true
}
</script>

<div id="reactionDiv"></div>

<script type="text/javascript">
   gigya.services.socialize.showReactionsBarUI(showReactionsBarUI_params);
</script>
