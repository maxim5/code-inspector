<?php  

if(preg_match('#' . basename(__FILE__) . '#', $_SERVER['PHP_SELF'])) { 	die('You are not allowed to call this page directly.'); }

// *** show main gallery list
function nggallery_manage_gallery_main() {

	global $ngg, $nggdb, $wp_query;
	
	if ( isset( $_POST['paged'] ))
		$_GET['paged'] = $_POST['paged'];
	
	//Build the pagination for more than 25 galleries
	if ( ! isset( $_GET['paged'] ) || $_GET['paged'] < 1 )
		$_GET['paged'] = 1;
	
	$gallery_name = $_GET['gallery_name'];
	$letter = $_GET['letter'];
	$author = $_GET['author'];
	$start = ( $_GET['paged'] - 1 ) * 25;

	if(empty($letter)){
		$letter = $_POST['letter'];
	}
	if(empty($author)){
		$author = $_POST['author'];
	}	
	
	//lvjiyun  
	$current_user = wp_get_current_user();
    $roles = $current_user->roles;
    if($roles[0] == 'celebrity'){
    	$author = $current_user->ID;
    }
	
	if(empty($gallery_name)){
		$gallery_name = $_POST['gallery_name'];
	}	
	$gallerylist = $nggdb->find_all_galleries('name', 'asc', TRUE, 25, $start, false, $letter, $author, $gallery_name);

	$args = array(
		'base' => add_query_arg( 'paged', '%#%' ),
		'format' => '',
		'prev_text' => __('&laquo;'),
		'next_text' => __('&raquo;'),
		'total' => $nggdb->paged['max_objects_per_page'],
		'current' => $_GET['paged']
	);
	if(!empty($letter)){
		$args = $args + array("add_args" => array("letter"=>$letter));
	}
	if(!empty($author)){
		$args = $args + array("add_args" => array("author"=>$author));
	}
	if(!empty($gallery_name)){
		$args = $args + array("add_args" => array("gallery_name"=>urlencode($gallery_name)));
	}
	$page_links = paginate_links($args);

	?>
	<script type="text/javascript"> 
	<!--
	function checkAll(form)
	{
		for (i = 0, n = form.elements.length; i < n; i++) {
			if(form.elements[i].type == "checkbox") {
				if(form.elements[i].name == "doaction[]") {
					if(form.elements[i].checked == true)
						form.elements[i].checked = false;
					else
						form.elements[i].checked = true;
				}
			}
		}
	}
	
	function getNumChecked(form)
	{
		var num = 0;
		for (i = 0, n = form.elements.length; i < n; i++) {
			if(form.elements[i].type == "checkbox") {
				if(form.elements[i].name == "doaction[]")
					if(form.elements[i].checked == true)
						num++;
			}
		}
		return num;
	}

	// this function check for a the number of selected images, sumbmit false when no one selected
	function checkSelected() {
	
		var numchecked = getNumChecked(document.getElementById('editgalleries'));
		 
		if(numchecked < 1) { 
			alert('<?php echo esc_js(__('No images selected', 'nggallery')); ?>');
			return false; 
		} 
		
		actionId = jQuery('#bulkaction').val();
		
		switch (actionId) {
			case "resize_images":
                showDialog('resize_images', '<?php echo esc_js(__('Resize images','nggallery')); ?>');
				return false;
				break;
			case "new_thumbnail":
				showDialog('new_thumbnail', '<?php echo esc_js(__('Create new thumbnails','nggallery')); ?>');
				return false;
				break;
		}
		
		return confirm('<?php echo sprintf(esc_js(__("You are about to start the bulk edit for %s galleries \n \n 'Cancel' to stop, 'OK' to proceed.",'nggallery')), "' + numchecked + '") ; ?>');
	}

	function showDialog( windowId, title ) {
		var form = document.getElementById('editgalleries');
		var elementlist = "";
		for (i = 0, n = form.elements.length; i < n; i++) {
			if(form.elements[i].type == "checkbox") {
				if(form.elements[i].name == "doaction[]")
					if(form.elements[i].checked == true)
						if (elementlist == "")
							elementlist = form.elements[i].value
						else
							elementlist += "," + form.elements[i].value ;
			}
		}
		jQuery("#" + windowId + "_bulkaction").val(jQuery("#bulkaction").val());
		jQuery("#" + windowId + "_imagelist").val(elementlist);
        // now show the dialog
    	jQuery( "#" + windowId ).dialog({
    		width: 640,
            resizable : false,
    		modal: true,
            title: title      
    	});
        jQuery("#" + windowId + ' .dialog-cancel').click(function() { jQuery( "#" + windowId ).dialog("close"); });
	}
	
	function showAddGallery() {
    	jQuery( "#addGallery").dialog({
    		width: 640,
            resizable : false,
    		modal: true,
            title: '<?php echo esc_js(__('Add new gallery','nggallery')); ?>'          
    	});	   
        jQuery("#addGallery .dialog-cancel").click(function() { jQuery( "#addGallery" ).dialog("close"); });
	}
	//-->
	</script>
	<div class="wrap">
        <?php screen_icon( 'nextgen-gallery' ); ?>
		<h2><?php echo _n( 'Gallery', 'Galleries', 2, 'nggallery'); ?></h2>
		<form class="search-form" action="" method="get">
		<p class="search-box">
			<label class="hidden" for="media-search-input"><?php _e( 'Search Images', 'nggallery' ); ?>:</label>
			<input type="hidden" id="page-name" name="page" value="nggallery-manage-gallery" />
			<input type="text" id="media-search-input" name="s" value="<?php the_search_query(); ?>" />
			<input type="submit" value="<?php _e( 'Search Images', 'nggallery' ); ?>" class="button" />
		</p>
		</form>
		<form id="editgalleries" class="nggform" method="POST" action="<?php echo $ngg->manage_page->base_page . '&amp;paged=' . $_GET['paged']; ?>" accept-charset="utf-8">
		<?php wp_nonce_field('ngg_bulkgallery') ?>
		<input type="hidden" name="page" value="manage-galleries" />
		
		<div class="tablenav">
			<?php 
			$current_user = wp_get_current_user();//lvjiyun   
    		$roles = $current_user->roles;//lvjiyun   
    		if($roles[0] != 'celebrity'){//lvjiyun   
			?>
			<div class="alignleft actions">
				<?php if ( function_exists('json_encode') ) : ?>
				<select name="bulkaction" id="bulkaction">
					<option value="no_action" ><?php _e("Bulk actions",'nggallery'); ?></option>
					<option value="delete_gallery" ><?php _e("Delete",'nggallery'); ?></option>
                    <option value="set_watermark" ><?php _e("Set watermark",'nggallery'); ?></option>
					<option value="new_thumbnail" ><?php _e("Create new thumbnails",'nggallery'); ?></option>
					<option value="resize_images" ><?php _e("Resize images",'nggallery'); ?></option>
					<option value="import_meta" ><?php _e("Import metadata",'nggallery'); ?></option>
					<option value="recover_images" ><?php _e("Recover from backup",'nggallery'); ?></option>
				</select>
				<input name="showThickbox" class="button-secondary" type="submit" value="<?php _e('Apply','nggallery'); ?>" onclick="if ( !checkSelected() ) return false;" />
				<?php endif; ?>
				<?php if ( current_user_can('NextGEN Upload images') && nggGallery::current_user_can( 'NextGEN Add new gallery' ) ) : ?>
					<input name="doaction" class="button-secondary action" type="submit" onclick="showAddGallery(); return false;" value="<?php _e('Add new gallery', 'nggallery') ?>"/>
				<?php endif; ?>
				&nbsp;&nbsp;letter:
				<select id="letterSelect" name="letter" onchange="window.location.href='/wp-admin/admin.php?page=nggallery-manage-gallery&letter='+this.options[this.options.selectedIndex].value+'&author='+document.getElementById('authorSelect').options[document.getElementById('authorSelect').options.selectedIndex].value;">
				<option value="">-- select --</option>
				<?php
				//lvjiyun
				$letter = $_GET['letter'];
				if(empty($letter)){
					$letter = $_POST['letter'];
				}
				foreach (range('a', 'z') as $l) {
					echo '<option value="'.$l.'"';
					if(!empty($letter) && $letter==$l){
						echo ' selected="selected"';
					}
					echo '>'.strtoupper($l).'</option>';
				}
				?>
				</select>
				<?php 
				global $wpdb;
				$sql = "select a.ID,a.display_name from wp_users a, wp_usermeta b where a.ID=b.user_id and b.meta_key='wp_capabilities' and (b.meta_value like '%\"editor\"%' or b.meta_value like '%\"administrator\"%')";
				$authors = $wpdb->get_results($sql);
				?>
				&nbsp;&nbsp;author:
				<select id="authorSelect" name="author" onchange="window.location.href='/wp-admin/admin.php?page=nggallery-manage-gallery&author='+this.options[this.options.selectedIndex].value+'&letter='+document.getElementById('letterSelect').options[document.getElementById('letterSelect').options.selectedIndex].value;">
				<option value="">-- select --</option>
				<?php
				//lvjiyun
				$author = $_GET['author'];
				if(empty($author)){
					$author = $_POST['author'];
				}
				foreach ($authors as $a) {
					echo '<option value="'.$a->ID.'"';
					if(!empty($author) && (int)$author==$a->ID){
						echo ' selected="selected"';
					}
					echo '>'.$a->display_name.'</option>';
				}
				?>
				</select>	
				<?php 
				$gallery_name = $_POST['gallery_name'];
				?>
				&nbsp;&nbsp;&nbsp;&nbsp;<input type="text" name="gallery_name" value="<?php echo $gallery_name;?>"/>&nbsp;<input type="submit" value="????"/>			
			</div>
			<?php } ?>
		<?php if ( $page_links ) : ?>
			<div class="tablenav-pages"><?php $page_links_text = sprintf( '<span class="displaying-num">' . __( 'Displaying %s&#8211;%s of %s' ) . '</span>%s',
				number_format_i18n( ( $_GET['paged'] - 1 ) * $nggdb->paged['objects_per_page'] + 1 ),
				number_format_i18n( min( $_GET['paged'] * $nggdb->paged['objects_per_page'], $nggdb->paged['total_objects'] ) ),
				number_format_i18n( $nggdb->paged['total_objects'] ),
				$page_links
			); echo $page_links_text; ?></div>
		<?php endif; ?>
		
		</div>
		<table class="widefat" cellspacing="0">
			<thead>
			<tr>
<?php print_column_headers('nggallery-manage-galleries'); ?>
			</tr>
			</thead>
			<tfoot>
			<tr>
<?php print_column_headers('nggallery-manage-galleries', false); ?>
			</tr>
			</tfoot>            
			<tbody>
<?php

if($gallerylist) {
    //get the columns
	$gallery_columns = ngg_manage_gallery_columns();
	$hidden_columns  = get_hidden_columns('nggallery-manage-images');
	$num_columns     = count($gallery_columns) - count($hidden_columns);
    
	foreach($gallerylist as $gallery) {
		$alternate = ( !isset($alternate) || $alternate == 'class="alternate"' ) ? '' : 'class="alternate"';
		$gid = $gallery->gid;
		$name = (empty($gallery->title) ) ? $gallery->name : $gallery->title;
		$author_user = get_userdata( (int) $gallery->author );
		?>
		<tr id="gallery-<?php echo $gid ?>" <?php echo $alternate; ?> >
		<?php 
		foreach($gallery_columns as $gallery_column_key => $column_display_name) {
			$class = "class=\"$gallery_column_key column-$gallery_column_key\"";
	
			$style = '';
			if ( in_array($gallery_column_key, $hidden_columns) )
				$style = ' style="display:none;"';
	
			$attributes = "$class$style";
			
			switch ($gallery_column_key) {
				case 'cb' :
					?>
        			<th scope="row" class="cb column-cb">
        				<?php if (nggAdmin::can_manage_this_gallery($gallery->author)) { ?>
        					<input name="doaction[]" type="checkbox" value="<?php echo $gid ?>" />
        				<?php } ?>
        			</th>
        			<?php 
    			break;
    			case 'id' :
    			    ?>
					<td <?php echo $attributes ?> scope="row"><?php echo $gid; ?></td>
					<?php 
    			break;
    			case 'title' :
    			    ?>
        			<td>
        				<?php if (nggAdmin::can_manage_this_gallery($gallery->author)) { ?>
        					<a href="<?php echo wp_nonce_url( $ngg->manage_page->base_page . '&amp;mode=edit&amp;gid=' . $gid, 'ngg_editgallery')?>" class='edit' title="<?php _e('Edit'); ?>" >
        						<?php echo nggGallery::i18n($name); ?>
        					</a>
        				<?php } else { ?>
        					<?php echo nggGallery::i18n($gallery->title); ?>
        				<?php } ?>
        			</td>
        			<?php 
    			break;
    			case 'description' :
    			    ?>
					<td <?php echo $attributes ?>><?php echo nggGallery::i18n($gallery->galdesc); ?>&nbsp;</td>
					<?php 
    			break;
    			case 'author' :
    			    ?>
					<td <?php echo $attributes ?>><?php echo $author_user->display_name; ?></td>
					<?php 
    			break;
    			case 'page_id' :
    			    ?>
        			<td <?php echo $attributes ?>><?php echo $gallery->pageid; ?></td>
        			<?php 
    			break;
    			case 'quantity' :
    			    ?>
        			<td <?php echo $attributes ?>><?php echo $gallery->counter; ?></td>
        			<?php 
    			break;
    			default : 
					?>
					<td <?php echo $attributes ?>><?php do_action('ngg_manage_gallery_custom_column', $gallery_column_key, $gid); ?></td>
					<?php
				break;
				}
	        } ?>
		</tr>
		<?php
	}
} else {
	echo '<tr><td colspan="7" align="center"><strong>' . __('No entries found', 'nggallery') . '</strong></td></tr>';
}
?>			
			</tbody>
		</table>
        <div class="tablenav">
		<?php if ( $page_links ) : ?>
			<div class="tablenav-pages"><?php $page_links_text = sprintf( '<span class="displaying-num">' . __( 'Displaying %s&#8211;%s of %s' ) . '</span>%s',
				number_format_i18n( ( $_GET['paged'] - 1 ) * $nggdb->paged['objects_per_page'] + 1 ),
				number_format_i18n( min( $_GET['paged'] * $nggdb->paged['objects_per_page'], $nggdb->paged['total_objects'] ) ),
				number_format_i18n( $nggdb->paged['total_objects'] ),
				$page_links
			); echo $page_links_text; ?></div>
		<?php endif; ?>
        </div>
		</form>
	</div>
	<!-- #addGallery -->
	<div id="addGallery" style="display: none;" >
		<form id="form-tags" method="POST" accept-charset="utf-8">
		<?php wp_nonce_field('ngg_addgallery'); ?>
		<input type="hidden" name="page" value="manage-galleries" />
		<table width="100%" border="0" cellspacing="3" cellpadding="3" >
		  	<tr>
		    	<td>
					<strong><?php _e('New Gallery', 'nggallery') ;?>:</strong> <input type="text" size="35" name="galleryname" value="" /><br />
					<?php if(!is_multisite()) { ?>
					<?php _e('Create a new , empty gallery below the folder', 'nggallery') ;?>  <strong><?php echo $ngg->options['gallerypath']; ?></strong><br />
					<?php } ?>
					<i>( <?php _e('Allowed characters for file and folder names are', 'nggallery') ;?>: a-z, A-Z, 0-9, -, _ )</i>
				</td>
		  	</tr>
		  	<tr align="right">
		    	<td class="submit">
		    		<input class="button-primary" type="submit" name="addgallery" value="<?php _e('OK','nggallery'); ?>" />
		    		&nbsp;
		    		<input class="button-secondary dialog-cancel" type="reset" value="&nbsp;<?php _e('Cancel', 'nggallery'); ?>&nbsp;" />
		    	</td>
			</tr>
		</table>
		</form>
	</div>
	<!-- /#addGallery -->

	<!-- #resize_images -->
	<div id="resize_images" style="display: none;" >
		<form id="form-resize-images" method="POST" accept-charset="utf-8">
		<?php wp_nonce_field('ngg_thickbox_form') ?>
		<input type="hidden" id="resize_images_imagelist" name="TB_imagelist" value="" />
		<input type="hidden" id="resize_images_bulkaction" name="TB_bulkaction" value="" />
		<input type="hidden" name="page" value="manage-galleries" />
		<table width="100%" border="0" cellspacing="3" cellpadding="3" >
			<tr valign="top">
				<td>
					<strong><?php _e('Resize Images to', 'nggallery'); ?>:</strong> 
				</td>
				<td>
					<input type="text" size="5" name="imgWidth" value="<?php echo $ngg->options['imgWidth']; ?>" /> x <input type="text" size="5" name="imgHeight" value="<?php echo $ngg->options['imgHeight']; ?>" />
					<br /><small><?php _e('Width x height (in pixel). NextGEN Gallery will keep ratio size','nggallery') ?></small>
				</td>
			</tr>
		  	<tr align="right">
		    	<td colspan="2" class="submit">
		    		<input class="button-primary" type="submit" name="TB_ResizeImages" value="<?php _e('OK', 'nggallery'); ?>" />
		    		&nbsp;
		    		<input class="button-secondary dialog-cancel" type="reset" value="&nbsp;<?php _e('Cancel', 'nggallery'); ?>&nbsp;" />
		    	</td>
			</tr>
		</table>
		</form>
	</div>
	<!-- /#resize_images -->

	<!-- #new_thumbnail -->
	<div id="new_thumbnail" style="display: none;" >
		<form id="form-new-thumbnail" method="POST" accept-charset="utf-8">
		<?php wp_nonce_field('ngg_thickbox_form') ?>
		<input type="hidden" id="new_thumbnail_imagelist" name="TB_imagelist" value="" />
		<input type="hidden" id="new_thumbnail_bulkaction" name="TB_bulkaction" value="" />
		<input type="hidden" name="page" value="manage-galleries" />
		<table width="100%" border="0" cellspacing="3" cellpadding="3" >
			<tr valign="top">
				<th align="left"><?php _e('Width x height (in pixel)','nggallery') ?></th>
				<td><input type="text" size="5" maxlength="5" name="thumbwidth" value="<?php echo $ngg->options['thumbwidth']; ?>" /> x <input type="text" size="5" maxlength="5" name="thumbheight" value="<?php echo $ngg->options['thumbheight']; ?>" />
				<br /><small><?php _e('These values are maximum values ','nggallery') ?></small></td>
			</tr>
			<tr valign="top">
				<th align="left"><?php _e('Set fix dimension','nggallery') ?></th>
				<td><input type="checkbox" name="thumbfix" value="1" <?php checked('1', $ngg->options['thumbfix']); ?> />
				<br /><small><?php _e('Ignore the aspect ratio, no portrait thumbnails','nggallery') ?></small></td>
			</tr>
		  	<tr align="right">
		    	<td colspan="2" class="submit">
		    		<input class="button-primary" type="submit" name="TB_NewThumbnail" value="<?php _e('OK', 'nggallery');?>" />
		    		&nbsp;
		    		<input class="button-secondary dialog-cancel" type="reset" value="&nbsp;<?php _e('Cancel', 'nggallery'); ?>&nbsp;" />
		    	</td>
			</tr>
		</table>
		</form>
	</div>
	<!-- /#new_thumbnail -->	

<?php
} 

// define the columns to display, the syntax is 'internal name' => 'display name'
function ngg_manage_gallery_columns() {
	
	$gallery_columns = array();
	
	$gallery_columns['cb'] = '<input name="checkall" type="checkbox" onclick="checkAll(document.getElementById(\'editgalleries\'));" />';
	$gallery_columns['id'] = __('ID');
	$gallery_columns['title'] = _n( 'Gallery', 'Galleries', 1, 'nggallery');
	$gallery_columns['description'] = __('Description', 'nggallery');
	$gallery_columns['author'] = __('Author', 'nggallery');
	$gallery_columns['page_id'] = __('Page ID', 'nggallery');
	$gallery_columns['quantity'] = _n( 'Image', 'Images', 2, 'nggallery' );

	$gallery_columns = apply_filters('ngg_manage_gallery_columns', $gallery_columns);

	return $gallery_columns;
}
?>