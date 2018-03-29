<?php
// $Id: bitcache.admin.inc,v 1.28 2009/07/01 04:09:48 arto Exp $

//////////////////////////////////////////////////////////////////////////////
// Bitcache settings

/**
 * Form builder: displays the Bitcache configuration screen.
 *
 * @ingroup forms
 * @see system_settings_form()
 */
function bitcache_admin_settings() {
  $form = array();

  // Server settings
  $form['server'] = array('#type' => 'fieldset', '#title' => t('Server settings'), '#collapsible' => TRUE, '#collapsed' => TRUE);
  $form['server']['bitcache_server'] = array(
    '#type'          => 'radios',
    '#title'         => t('Bitcache REST API'),
    '#default_value' => (int)BITCACHE_SERVER,
    '#options'       => array(t('Disabled'), t('Enabled')),
    '#description'   => t('When enabled, the built-in Bitcache server is made available at !url. Access to the <a href="http://en.wikipedia.org/wiki/Representational_State_Transfer" target="_blank" title="Representational State Transfer">REST</a> <acronym title="Application Programming Interface">API</acronym> is subject to Drupal <a href="@manage-users">authentication</a> and <a href="@manage-perms">permissions</a>.', array('!url' => l(BITCACHE_URL, BITCACHE_URL), '@manage-users' => url('admin/user'), '@manage-perms' => url('admin/user/permissions', array('fragment' => 'module-bitcache')))),
  );
  $form['server']['bitcache_features'] = array(
    '#type'          => 'checkboxes',
    '#title'         => t('Enabled features'),
    '#default_value' => bitcache_get_features(),
    '#options'       => array('index' => t('INDEX'), 'get' => t('GET/HEAD'), 'post' => t('POST'), 'put' => t('PUT'), 'delete' => t('DELETE')),
    '#description'   => t('Determines which HTTP methods will be supported in the <a href="http://bitcache.org/api/rest" target="_blank">REST API</a>. For finer-grained access controls based on user credentials, refer to the Drupal <a href="@manage-perms">permissions</a> for Bitcache.', array('@manage-perms' => url('admin/user/permissions', array('fragment' => 'module-bitcache')))),
  );
  $form['server']['bitcache_transfer_method'] = array(
    '#type'          => 'select',
    '#title'         => t('Download method'),
    '#default_value' => BITCACHE_TRANSFER_METHOD,
    '#options'       => array('redirect' => t('Redirect (external)'), 'xsendfile' => t('Redirect (internal X-Sendfile)'), 'readfile' => t('Passthrough (unbuffered)'), 'read4k' => t('Passthrough (4K chunks)')),
    '#description'   => t('Determines how the bitstream contents will be transferred to the client when a download is requested. Note that this is a default setting only, and the actual transfer method may in practice be determined by the repository storage location.'),
  );

  // Bitcache modules
  $form['modules'] = array('#type' => 'fieldset', '#title' => t('Bitcache modules'), '#collapsible' => TRUE, '#collapsed' => TRUE);
  if (count(bitcache_get_modules()) > 0) {
    $head = array(array('data' => t('Name'), 'width' => '30%'), t('Description'));
    $rows = array();
    foreach (bitcache_get_modules('info') as $name => $info) {
      $rows[] = array($info->title, array('data' => $info->description, 'class' => 'description'));
    }
    $modules = theme('table', $head, $rows);
  }
  else {
    $modules = '<div>' . t('No Bitcache modules currently installed.') . '</div>';
  }
  $form['modules']['bitcache_modules'] = array('#type' => 'markup', '#value' => $modules);

  // Bitcache adapters
  $form['adapters'] = array('#type' => 'fieldset', '#title' => t('Bitcache adapters'), '#collapsible' => TRUE, '#collapsed' => TRUE);
  if (count(bitcache_get_adapters()) > 0) {
    $head = array(array('data' => t('Name'), 'width' => '30%'), t('Description'));
    $rows = array();
    foreach (bitcache_get_adapters('info') as $name => $info) {
      $row = array($info->title, array('data' => $info->description, 'class' => 'description'));
      // Show greyed-out text for adapters that are not available (e.g. due to missing PHP extensions)
      $rows[] = !empty($info->enabled) ? $row : array('data' => $row, 'class' => 'menu-disabled');
    }
    $adapters = theme('table', $head, $rows);
  }
  else {
    $adapters = '<div>' . t('No Bitcache adapters currently available.') . '</div>';
  }
  $form['adapters']['bitcache_adapters'] = array('#type' => 'markup', '#value' => $adapters);

  // Bitcache tools
  $form['tools'] = array('#type' => 'fieldset', '#title' => t('Bitcache tools'), '#collapsible' => TRUE, '#collapsed' => TRUE);
  $form['tools']['bitcache_exec'] = array(
    '#type'          => 'textfield',
    '#title'         => t('Path to Bitcache executable'),
    '#default_value' => BITCACHE_EXEC,
    '#size'          => 60,
    '#maxlength'     => 255,
    '#required'      => FALSE,
    '#description'   => t('A file system path to the <code>bit</code> binary. On Unix systems, this would typically be located at <code>/usr/bin/bit</code> or <code>/usr/local/bin/bit</code>. On Mac OS X with MacPorts, the path would typically be <code>/opt/local/bin/bit</code>. In case you have installed the command-line tools locally under your Drupal installation according to <code>INSTALL.txt</code>, the path would be <code>!path</code>.', array('!path' => drupal_get_path('module', 'bitcache') . '/tools/bin/bit'))
  );

  // Cron settings
  $form['cron'] = array('#type' => 'fieldset', '#title' => t('Cron script'), '#collapsible' => TRUE, '#collapsed' => TRUE);
  $form['cron']['bitcache_cron'] = array('#type' => 'textfield', '#title' => t('Shell command to execute on cron runs'), '#default_value' => BITCACHE_CRON, '#maxlength' => 255, '#description' => t('To synchronize the local Bitcache repository with a remote location, enter an <em>rsync</em> or <em>bit-sync</em> command that should be triggered during cron maintenance runs. Leave empty to disable this functionality.'), '#required' => FALSE);
  // TODO: execute only if creates/deletes vs. execute regardless

  // Stream wrappers
  $wrappers = implode(', ', stream_get_wrappers()); // requires PHP 5
  $form['wrappers'] = array('#type' => 'fieldset', '#title' => t('Stream wrappers'), '#collapsible' => TRUE, '#collapsed' => TRUE);
  $form['wrappers']['list'] = array(
    '#type'          => 'markup',
    '#value'         => '<div class="form-item"><label>' . t('Available stream wrappers') . ':</label>' . $wrappers . '<div class="description">' . t('This is the list of currently registered <a href="http://php.net/stream">PHP stream wrappers</a> that can be used when specifying storage locations for file system <a href="@manage-repos">repositories</a>.', array('@manage-repos' => url('admin/settings/bitcache/repositories'))) . '</div></div>',
  );

  return system_settings_form($form);
}

//////////////////////////////////////////////////////////////////////////////
// Bitcache bitstream management

function bitcache_admin_bitstreams(&$form_state, $name = NULL, $max_count = 100) {
  drupal_add_css(drupal_get_path('module', 'bitcache') . '/bitcache.css');

  $head = array(t('ID'), t('Kind'), array('data' => t('Size'), 'class' => 'size'), array('data' => t('Operations'), 'colspan' => 2));
  $rows = array();

  $repo = bitcache_get_repository($name);
  foreach ($repo as $id => $stream) {
    $rows[$id] = array(
      l($id, 'admin/content/bitcache/view/' . $id),
      $stream->type(),
      array('data' => format_size($stream->size()), 'class' => 'size'),
      bitcache_l('download', $id),
      l('delete', 'admin/content/bitcache/delete/' . $id),
    );
    if (@++$count == $max_count) {
      break;
    }
  }
  ksort($rows);
  $rows = array_values($rows);

  if (empty($rows)) {
    $rows[] = array(array('data' => t('No bitstreams available.'), 'colspan' => '5'));
  }

  $form['table'] = array('#type' => 'markup', '#value' => theme('table', $head, $rows, array('class' => 'bitcache')));
  return $form;
}

function theme_bitcache_admin_bitstreams($form) {
  return drupal_render($form); // TODO
}

function bitcache_admin_bitstream_upload(&$form_state, $edit = array('attach' => '', 'repository' => 'bitcache')) {
  $edit = (object)$edit;
  $form = array();

  $form['upload'] = array('#type' => 'fieldset', '#title' => t('Upload file'));
  $form['upload']['attach'] = array(
    '#type' => 'file', 
    '#title' => '',
    '#default_value' => $edit->attach,
    '#description' => t(''), // TODO
  );

  $form['upload']['repository'] = array(
    '#type' => 'select',
    '#title' => t('Repository'),
    '#default_value' => $edit->repository,
    '#options' => bitcache_get_repositories('titles'),
    '#description' => t('Select the Bitcache repository that will be used to store the uploaded bitstream.'),
  );

  $form['submit'] = array('#type' => 'submit', '#value' => t('Upload bitstream'));
  $form['#attributes']['enctype'] = 'multipart/form-data';
  return $form;
}

function bitcache_admin_bitstream_upload_validate($form, &$form_state) {
  extract($form_state['values'], EXTR_SKIP | EXTR_REFS);
  $source = 'attach';

  if (!isset($_FILES['files']) || !$_FILES['files']['name'][$source] || !is_uploaded_file($_FILES['files']['tmp_name'][$source])) {
    form_set_error($source, t('File upload required.'));
  }
}

function bitcache_admin_bitstream_upload_submit($form, &$form_state) {
  extract($form_state['values'], EXTR_SKIP | EXTR_REFS);
  $source = 'attach';

  if (isset($_FILES['files']) && $_FILES['files']['name'][$source] && is_uploaded_file($_FILES['files']['tmp_name'][$source])) {
    bitcache_use_repository($repository);
    if (!($id = bitcache_put_file(NULL, $_FILES['files']['tmp_name'][$source], TRUE))) {
      watchdog('bitcache', 'Upload error. Could not move uploaded file %file to repository.', array('%file' => $_FILES['files']['tmp_name'][$source]), WATCHDOG_ERROR);
      return;
    }
    watchdog('bitcache', 'Bitstream uploaded: %id.', array('%id' => $id), WATCHDOG_NOTICE, l(t('view'), 'admin/content/bitcache'));
    drupal_set_message(t('The bitstream %id was successfully uploaded.', array('%id' => $id)));
  }

  $form_state['redirect'] = 'admin/content/bitcache';
}

function bitcache_admin_bitstream_fetch(&$form_state, $edit = array('url' => 'http://', 'repository' => 'bitcache')) {
  $edit = (object)$edit;
  $form = array();

  set_time_limit(0); // let's take all the time we need

  $form['fetch'] = array('#type' => 'fieldset', '#title' => t('Fetch URL'));
  $form['fetch']['url'] = array(
    '#type' => 'textfield',
    '#title' => '',
    '#default_value' => $edit->url,
    '#maxlength' => 255,
    '#description' => t(''),
    '#required' => FALSE,
  );

  $form['fetch']['repository'] = array(
    '#type' => 'select',
    '#title' => t('Repository'),
    '#default_value' => $edit->repository,
    '#options' => bitcache_get_repositories('titles'),
    '#description' => t('Select the Bitcache repository that will be used to store the fetched bitstream.'),
  );

  $form['submit'] = array('#type' => 'submit', '#value' => t('Fetch bitstream'));
  return $form;
}

function bitcache_admin_bitstream_fetch_validate($form, &$form_state) {
  extract($form_state['values'], EXTR_SKIP | EXTR_REFS);

  $url = trim($url);
  if (!valid_url($url) || !@parse_url($url)) {
    form_set_error('url', t('Malformed or invalid URL.'));
  }
}

function bitcache_admin_bitstream_fetch_submit($form, &$form_state) {
  extract($form_state['values'], EXTR_SKIP | EXTR_REFS);

  if (preg_match('!^(http|https)://!', $url)) { // HTTP or HTTPS
    if ($response = @drupal_http_request($url)) {
      if (isset($response->error)) {
        form_set_error('url', t('Error fetching %url: @error', array('%url' => $url, '@error' => $response->error)));
        return;
      }
      $data = $response->data;
    }
  }
  else {
    if (($data = file_get_contents($url)) === FALSE) {
      form_set_error('url', t('Error fetching %url.', array('%url' => $url)));
      return;
    }
  }

  if (isset($data)) {
    bitcache_use_repository($repository);
    if (!($id = bitcache_put(NULL, $data))) {
      watchdog('bitcache', 'Upload error. Could not store contents of %url to repository.', array('%url' => $url), WATCHDOG_ERROR);
      return;
    }
    watchdog('bitcache', 'Bitstream uploaded: %id.', array('%id' => $id), WATCHDOG_NOTICE, l(t('view'), 'admin/content/bitcache'));
    drupal_set_message(t('The bitstream %id was successfully fetched.', array('%id' => $id)));
  }

  $form_state['redirect'] = 'admin/content/bitcache';
}

function bitcache_admin_bitstream_view($id) {
  if (!bitcache_exists($id)) {
    return drupal_not_found();
  }

  $blob = bitcache_get($id);

  $rows = array(
    array(
      array('data' => t('Repository'), 'header' => TRUE),
      t('Default'), // FIXME
    ),
    array(
      array('data' => t('ID'), 'header' => TRUE),
      t('@id (%algo)', array('@id' => $id, '%algo' => 'SHA-1')), // FIXME
    ),
    array(
      array('data' => t('Size'), 'header' => TRUE),
      format_size($blob->size()),
    ),
    array(
      array('data' => t('MIME type'), 'header' => TRUE),
      check_plain($blob->type()),
    ),
    array(
      array('data' => t('Created'), 'header' => TRUE),
      format_date(@filectime($blob->path), 'large'), // FIXME
    ),
    array(
      array('data' => t('Compressed'), 'header' => TRUE),
      !empty($blob->compressed) ? t('Yes') : t('No'),
    ),
    array(
      array('data' => t('Encrypted'), 'header' => TRUE),
      !empty($blob->encrypted) ? t('Yes') : t('No'),
    ),
  );

  return theme('table', array(), $rows, array('class' => 'bitcache bitstream'));
}

function bitcache_admin_bitstream_delete($form_state, $id) {
  $form['id'] = array('#type' => 'value', '#value' => $id);

  return confirm_form($form,
    t('Are you sure you want to delete the bitstream %id?', array('%id' => $id)),
    isset($_GET['destination']) ? $_GET['destination'] : 'admin/content/bitcache',
    t('This action cannot be undone.'));
}

function bitcache_admin_bitstream_delete_submit($form, &$form_state) {
  extract($form_state['values'], EXTR_SKIP | EXTR_REFS);

  if ($form_state['values']['confirm']) {
    bitcache_delete($id);

    watchdog('bitcache', 'Bitstream deleted: %id.', array('%id' => $id));
    drupal_set_message(t('The bitstream %id has been deleted.', array('%id' => $id)));
  }

  $form_state['redirect'] = 'admin/content/bitcache';
}

//////////////////////////////////////////////////////////////////////////////
// Bitcache repository management

/**
 * Menu callback: displays the Bitcache repository management screen.
 */
function bitcache_admin_repos(&$form_state) {
  $form = array('#tree' => TRUE);

  $adapters = bitcache_get_adapters('titles');
  foreach (bitcache_get_repositories('info') as $name => $info) {
    $available = bitcache_has_adapter($info['adapter']);
    $configurable = ($name != 'bitcache') && empty($info['module']);

    $form[$name]['title']   = array('#value' => !$available ? $info['title'] : l($info['title'], 'admin/content/bitcache/' . $name, array('attributes' => array('title' => @$info['description']))));
    $form[$name]['adapter'] = array('#value' => isset($adapters[$info['adapter']]) ? $adapters[$info['adapter']] : $info['adapter']);
    $form[$name]['count']   = array('#value' => !$available ? '-' : number_format(bitcache_get_repository_count($name)));
    $form[$name]['size']    = array('#value' => !$available ? '-' : format_size(bitcache_get_repository_size($name)));
    $form[$name]['weight']  = array('#type' => 'weight', '#delta' => 50, '#default_value' => $info['weight']);
    $form[$name]['edit']    = array('#value' => !$available || !$configurable ? '' : l(t('configure'), 'admin/settings/bitcache/repository/edit/' . $name));
    $form[$name]['delete']  = array('#value' => !$configurable ? '' : l(t('delete'), 'admin/settings/bitcache/repository/delete/' . $name));
    $form[$name]['#class']  = !$available ? 'menu-disabled' : '';
  }

  $form['buttons']['submit'] = array('#type' => 'submit', '#value' => t('Save configuration'));
  $form['buttons']['reset']  = array('#type' => 'submit', '#value' => t('Reset to defaults'));
  return $form;
}

function theme_bitcache_admin_repos($form) {
  $head = array(t('Name'), t('Kind'), t('Total Items'), t('Total Size'), t('Weight'), array('data' => t('Operations'), 'colspan' => '2'));
  $rows = array();

  foreach (element_children($form) as $key) {
    if (($row = &$form[$key]) && !isset($row['title'])) {
      continue;
    }

    $row['weight']['#attributes']['class'] = 'repository-weight';
    $rows[] = array(
      'data' => array(
        drupal_render($row['title']),
        drupal_render($row['adapter']),
        drupal_render($row['count']),
        drupal_render($row['size']),
        drupal_render($row['weight']),
        drupal_render($row['edit']),
        drupal_render($row['delete']),
      ),
      'class' => trim('draggable' . ' ' . $row['#class']),
    );
  }

  $form['#value'] = theme('table', $head, $rows, array('id' => 'bitcache-repositories', 'class' => 'bitcache repositories'));
  drupal_add_tabledrag('bitcache-repositories', 'order', 'sibling', 'repository-weight');

  return drupal_render($form);
}

function bitcache_admin_repos_submit($form, &$form_state) {
  extract($form_state['values'], EXTR_SKIP | EXTR_REFS);

  if ($op == $buttons['reset']) {
    db_query("UPDATE {bitcache_repositories} SET weight = 0"); // reset weights
    db_query("UPDATE {bitcache_repositories} SET weight = -10 WHERE name = '%s'", 'bitcache');
    db_query("UPDATE {bitcache_repositories} SET weight = -1 WHERE name = '%s'", 'default');
    drupal_set_message(t('The configuration options have been reset to their default values.'));
    return;
  }

  foreach ($form_state['values'] as $name => $row) {
    if (isset($row['weight'])) {
      db_query("UPDATE {bitcache_repositories} SET weight = %d WHERE name = '%s'", (int)$row['weight'], $name);
    }
    unset($form_state['values'][$name]);
  }
  drupal_set_message(t('The configuration options have been saved.'));
}

function bitcache_admin_repo_create($adapter = 'sql') {
  return drupal_get_form('bitcache_admin_repo_form', array('name' => '', 'title' => '', 'description' => '', 'adapter' => $adapter, 'location' => BITCACHE_ROOT . '/'));
}

function bitcache_admin_repo_edit($name) {
  return drupal_get_form('bitcache_admin_repo_form', bitcache_get_repository($name)->options);
}

function bitcache_admin_repo_form(&$form_state, $edit = array('name' => '', 'title' => '', 'description' => '', 'adapter' => 'sql', 'location' => '')) {
  $edit = (object)$edit;
  $form = array();

  // Repository identification
  $form['identity'] = array('#type' => 'fieldset', '#title' => t('Repository identification'));
  $form['identity']['title'] = array(
    '#type'          => 'textfield',
    '#title'         => t('Title'),
    '#default_value' => $edit->title,
    '#maxlength'     => 64,
    '#description'   => t('The human-readable name of this repository. This text will be displayed as part of the list on the repository management page. This name must begin with a capital letter and contain only letters, numbers, and spaces. This name must be unique.'),
    '#required'      => TRUE,
    '#disabled'      => ($edit->name == 'bitcache'),
  );
  $form['identity']['name'] = array(
    '#type'          => 'textfield',
    '#title'         => t('Name'),
    '#default_value' => $edit->name,
    '#maxlength'     => 64,
    '#description'   => t('The machine-readable name of this repository. This name must contain only lowercase letters, numbers, and underscores. This name must be unique.'),
    '#required'      => TRUE,
    '#disabled'      => ($edit->name == 'bitcache'),
  );
  $form['identity']['description'] = array(
    '#title'         => t('Description'),
    '#type'          => 'textarea',
    '#default_value' => $edit->description,
    '#rows'          => 2,
    '#description'   => t('A brief description of this repository. This is only used for administrative purposes, so you can leave this empty if you wish.'),
    '#disabled'      => ($edit->name == 'bitcache'),
  );
  drupal_add_js(array('bitcache' => array('locationBase' => !empty($edit->location) ? $edit->location : '', 'locationChanged' => FALSE)), 'setting');
  if (empty($edit->name)) { // only for new repositories
    drupal_add_js('if (Drupal.jsEnabled) { $(document).ready(function() { $("#edit-name").keyup(function() { if (!Drupal.settings.bitcache.locationChanged) { var path = Drupal.settings.bitcache.locationBase + this.value; $("#edit-file-location").val(path); $("#edit-gdbm-location").val(path + ".db"); } }); }); }', 'inline');
  }

  // Repository storage
  $form['storage'] = array('#type' => 'fieldset', '#title' => t('Repository storage'));
  $form['storage']['adapter'] = array(
    '#type'          => 'select',
    '#title'         => t('Storage adapter'),
    '#default_value' => !empty($edit->adapter) ? $edit->adapter : 'sql',
    '#options'       => bitcache_get_adapters('titles', TRUE),
    '#description'   => t('Select a repository <a href="http://bitcache.org/faq/storage-adapters" target="_blank">storage adapter</a>. This determines how and where the repository content is stored.'),
    '#disabled'      => !empty($edit->name), // only enabled when creating repository
  );

  // Repository storage (PDO)
  if (bitcache_has_adapter('pdo')) {
    $form['storage']['pdo'] = array(
      '#prefix'        => '<div id="edit-pdo-wrapper" style="display: ' . ((!empty($edit->adapter) && $edit->adapter != 'pdo') ? 'none' : 'block') . ';">',
      '#suffix'        => '</div>',
      '#tree'          => TRUE,
    );
    $form['storage']['pdo']['dsn'] = array(
      '#type'          => 'textfield',
      '#title'         => t('DSN'),
      '#default_value' => !empty($edit->dsn) ? $edit->dsn : '',
      '#maxlength'     => 255,
      '#description'   => t('The Data Source Name (DSN) that contains the information required to connect to the database. You can use any of the available <a href="http://php.net/manual/en/pdo.drivers.php" target="_blank">PDO drivers</a>. For <a href="http://php.net/manual/en/ref.pdo-sqlite.php" target="_blank">SQLite</a>, you can use <code>sqlite::memory:</code> for a non-persistent in-memory database, or <code>sqlite:/path/to/database.sqlite</code> for a persistent database file. For <a href="http://php.net/manual/en/ref.pdo-mysql.php" target="_blank">MySQL</a>, specify the DSN in a form that includes the user name and password, as in <code>mysql:host=localhost;dbname=mydb;user=myuser;password=mypasswd</code>. Note that by default (regardless of the PDO driver used), data will be stored in a database table named %table, which will be created if it doesn\'t exist; you can override this by specifying <code>;table=mytable</code> as part of the DSN.', array('%table' => BITCACHE_TABLE_DEFAULT)),
      '#required'      => FALSE,
    );
    drupal_add_js('if (Drupal.jsEnabled) { $(document).ready(function() { $("#edit-adapter").change(function() { $("#edit-pdo-wrapper").css("display", (this.value == "pdo" ? "block" : "none")); }); }); }', 'inline');
  }

  // Repository storage (GDBM)
  if (bitcache_has_adapter('gdbm')) {
    $form['storage']['gdbm'] = array(
      '#prefix'        => '<div id="edit-gdbm-wrapper" style="display: ' . ((!empty($edit->adapter) && $edit->adapter != 'gdbm') ? 'none' : 'block') . ';">',
      '#suffix'        => '</div>',
      '#tree'          => TRUE,
    );
    $form['storage']['gdbm']['location'] = array(
      '#type'          => 'textfield',
      '#title'         => t('Location'),
      '#default_value' => !empty($edit->location) ? $edit->location : '',
      '#maxlength'     => 255,
      '#description'   => t('The file system path to the GDBM file. If the file doesn\'t exist, it will be created provided the directory is writable.'),
      '#required'      => FALSE,
    );
    drupal_add_js('if (Drupal.jsEnabled) { $(document).ready(function() { $("#edit-adapter").change(function() { $("#edit-gdbm-wrapper").css("display", (this.value == "gdbm" ? "block" : "none")); }); }); }', 'inline');
    drupal_add_js('if (Drupal.jsEnabled) { $(document).ready(function() { $("#edit-gdbm-location").keypress(function() { Drupal.settings.bitcache.locationChanged = true; }); }); }', 'inline');
  }

  // Repository storage (File system)
  if (bitcache_has_adapter('file')) {
    $form['storage']['file'] = array(
      '#prefix'        => '<div id="edit-file-wrapper" style="display: ' . (empty($edit->location) || (!empty($edit->adapter) && $edit->adapter != 'file') ? 'none' : 'block') . ';">',
      '#suffix'        => '</div>',
      '#tree'          => TRUE,
    );
    $form['storage']['file']['location'] = array(
      '#type'          => 'textfield',
      '#title'         => t('Location'),
      '#default_value' => !empty($edit->location) ? $edit->location : '',
      '#maxlength'     => 255,
      '#description'   => t('The storage location for this repository, given either as a local file system path or a full URL making use of any of the available <a href="http://php.net/manual/en/book.stream.php" target="_blank">PHP stream wrappers</a>. Typically you would place this directory under the defalt Bitcache directory %path.', array('%path' => BITCACHE_ROOT)),
      '#required'      => FALSE,
    );
    drupal_add_js('if (Drupal.jsEnabled) { $(document).ready(function() { $("#edit-adapter").change(function() { $("#edit-file-wrapper").css("display", (this.value == "file" ? "block" : "none")); }); }); }', 'inline');
    drupal_add_js('if (Drupal.jsEnabled) { $(document).ready(function() { $("#edit-file-location").keypress(function() { Drupal.settings.bitcache.locationChanged = true; }); }); }', 'inline');
  }

  // Repository storage (Amazon S3)
  if (bitcache_has_adapter('aws_s3')) {
    $form['storage']['aws_s3'] = array(
      '#prefix'        => '<div id="edit-aws_s3-wrapper" style="display: ' . ((!empty($edit->adapter) && $edit->adapter != 'aws_s3') ? 'none' : 'block') . ';">',
      '#suffix'        => '</div>',
      '#tree'          => TRUE,
    );
    $form['storage']['aws_s3']['bucket'] = array(
      '#type'          => 'textfield',
      '#title'         => t('Bucket name'),
      '#default_value' => !empty($edit->bucket) ? $edit->bucket : '',
      '#maxlength'     => 255,
      '#description'   => t('<a href="https://s3.amazonaws.com/" target="_blank">Amazon S3</a> stores bitstreams in uniquely-named "buckets". Bucket names are used to form the URL that bitstreams can be accessed at, such as <code>https://BUCKET.s3.amazonaws.com/</code>. Buckets can be located either in the United States or in Europe, and access to buckets can be either public or private. If the specified bucket doesn\'t exist, it will be created when it is first accessed.</code>'),
      '#required'      => FALSE,
    );
    $form['storage']['aws_s3']['acl'] = array(
      '#type'          => 'select',
      '#title'         => t('Access control policy'),
      '#default_value' => !empty($edit->bucket) ? $edit->bucket : 'public-read',
      '#options'       => array('public-read' => t('Owner read/write, public read'), 'public-read-write' => t('Owner read/write, public read/write'), 'private' => t('Owner read/write, public none')),
      '#description'   => t('This access control policy is applied to the bucket itself (if it does not exist yet), as well as to any new bitstreams stored into the bucket. To modify access policies for previously-existing buckets or bitstreams, use client software such as <a href="http://www.s3fox.net/" target="_blank">S3Fox</a>.'),
    );
    $form['storage']['aws_s3']['access_key'] = array(
      '#type'          => 'textfield',
      '#title'         => t('Access key'),
      '#default_value' => !empty($edit->access_key) ? $edit->access_key : '',
      '#maxlength'     => 255,
      '#description'   => t(''), // TODO
      '#required'      => FALSE,
    );
    $form['storage']['aws_s3']['secret_key'] = array(
      '#type'          => 'textfield',
      '#title'         => t('Secret key'),
      '#default_value' => !empty($edit->secret_key) ? $edit->secret_key : '',
      '#maxlength'     => 255,
      '#description'   => t(''), // TODO
      '#required'      => FALSE,
    );
    drupal_add_js('if (Drupal.jsEnabled) { $(document).ready(function() { $("#edit-adapter").change(function() { $("#edit-aws_s3-wrapper").css("display", (this.value == "aws_s3" ? "block" : "none")); }); }); }', 'inline');
  }

  // Repository storage > Content identification
  $form['storage']['id'] = array('#type' => 'fieldset', '#title' => t('Content identification'), '#collapsible' => TRUE, '#collapsed' => !empty($edit->name) || TRUE); // FIXME
  $form['storage']['id']['#description'] = t('<a href="http://en.wikipedia.org/wiki/Bitstream" target="_blank">Bitstreams</a> are uniquely identified and addressed by a <a href="http://bitcache.org/faq/digital-fingerprint" target="_blank">digital fingerprint</a>.');
  $form['storage']['id']['fingerprint'] = array(
    '#type'          => 'select',
    '#title'         => t('Fingerprint algorithm'),
    '#default_value' => !empty($edit->fingerprint) ? $edit->fingerprint : 'sha1',
    '#options'       => bitcache_get_algorithms('fingerprint'),
    '#description'   => t('Select the <a href="http://en.wikipedia.org/wiki/Cryptographic_hash_function" target="_blank">cryptographic hash algorithm</a> that will be used to compute digital fingerprints for bitstream contents.'),
  );
  $form['storage']['id']['encode'] = array(
    '#type'          => 'select',
    '#title'         => t('Fingerprint encoding'),
    '#default_value' => !empty($edit->encode) ? $edit->encode : 'base16',
    '#options'       => bitcache_get_algorithms('encode'),
    '#description'   => t('Select the encoding method that bitstream fingerprints will be displayed in.'),
  );

  // Repository storage > Content storage
  $form['storage']['content'] = array('#type' => 'fieldset', '#title' => t('Content storage'), '#collapsible' => TRUE, '#collapsed' => !empty($edit->name) || TRUE); // FIXME
  $form['storage']['content']['#description'] = t('Bitstream contents can optionally be <a href="http://en.wikipedia.org/wiki/Data_compression" target="_blank">compressed</a> (optimizing the use of storage space at the cost of some performance) and/or <a href="http://en.wikipedia.org/wiki/Encryption" target="_blank">encrypted</a> (securing repository contents against compromised storage, again at a performance cost).');
  $form['storage']['content']['compress'] = array(
    '#type'          => 'select',
    '#title'         => t('Compression algorithm'),
    '#default_value' => !empty($edit->compress) ? $edit->compress : '',
    '#options'       => array_merge(array('' => t('<no compression>')), bitcache_get_algorithms('compress')),
    '#description'   => t('Select a compression algorithm that will be applied to bitstream contents before they are written to the repository.'),
  );
  $form['storage']['content']['encrypt'] = array(
    '#type'          => 'select',
    '#default_value' => !empty($edit->encrypt) ? $edit->encrypt : '',
    '#options'       => array_merge(array('' => t('<no encryption>')), bitcache_get_algorithms('encrypt')),
    '#title'         => t('Encryption algorithm'),
    '#description'   => t('Select an encryption algorithm that will be applied to bitstream contents before they are written to the repository. Note that if you have also selected a compression algorithm, bitstream contents are always compressed first and only then encrypted.'),
  );

  if (!empty($edit->name) || TRUE) { // FIXME
    foreach (element_children($form['storage']['id']) as $field) {
      $form['storage']['id'][$field]['#attributes']['disabled'] = 'disabled';
    }
    foreach (element_children($form['storage']['content']) as $field) {
      $form['storage']['content'][$field]['#attributes']['disabled'] = 'disabled';
    }
  }

  // TODO: Policy settings
  // Minimum bitstream size
  // Maximum bitstream size

  if ($edit->name != 'bitcache') {
    $form['key'] = array('#type' => 'hidden', '#value' => $edit->name);
    $form['submit'] = array('#type' => 'submit', '#value' => empty($edit->name) ? t('Create new repository') : t('Update repository settings'));
  }
  return $form;
}

function bitcache_admin_repo_form_validate($form, &$form_state) {
  extract($form_state['values'], EXTR_SKIP | EXTR_REFS);

  if (!preg_match('/^[a-z]+[a-z\d_]*$/', $name)) {
    form_set_error('name', t('The machine-readable name can only consist of lowercase letters, underscores, and numbers.', array('%name' => $name)));
  }

  if (empty($key)) {
    // Prevent duplicate names for repositories:
    if (array_search($name, bitcache_get_repositories('names')) !== FALSE) {
      form_set_error('name', t('The machine-readable name %name is already used by another repository.', array('%name' => $name)));
    }

    if ($adapter == 'file') {
      // Prevent duplicate paths for repositories:

      $file['location'] = rtrim(trim($file['location']), '/\\');

      if (!trim($file['location'])) {
        form_set_error('file][location', t('Location field is required.'));
      }

      foreach (bitcache_get_repositories('info') as $info) {
        if ($info['adapter'] == $adapter && $info['location'] == $file['location']) {
          form_set_error('file][location', t('The location %path is already used by another repository.', array('%path' => $file['location'])));
          break;
        }
      }

      // Ensure that the directory can be created:
      if (!is_dir($location = bitcache_token_replace($file['location'])) && !@mkdir($location)) {
        form_set_error('file][location', t('The directory %path could not be created.', array('%path' => $location)));
      }
    }
  }
}

function bitcache_admin_repo_form_submit($form, &$form_state) {
  extract($form_state['values'], EXTR_SKIP | EXTR_REFS);

  $options = call_user_func_array('compact', array_merge(array('title', 'description', 'adapter', 'fingerprint', 'encode', 'compress', 'encrypt')));
  $options = array_merge($options, isset($$adapter) ? $$adapter : array());

  if (empty($key)) {
    bitcache_create_repository($name, $options);
    drupal_set_message(t('The repository has been created.'));
  }
  else {
    if ($key != $name) {
      bitcache_rename_repository($key, $name);
    }
    bitcache_update_repository($name, $options);
    drupal_set_message(t('The repository settings have been updated.'));
  }

  $form_state['redirect'] = 'admin/settings/bitcache/repositories';
}

function bitcache_admin_repo_delete($form_state, $name) {
  $form['name'] = array('#type' => 'value', '#value' => $name);
  $output = confirm_form($form,
    t('Are you sure you want to delete the repository %title?', array('%title' => $name)),
    isset($_GET['destination']) ? $_GET['destination'] : 'admin/settings/bitcache/repositories',
    t('This action will destroy all data contained in the repository and cannot be undone.'));
  return $output;
}

function bitcache_admin_repo_delete_submit($form, &$form_state) {
  if ($form_state['values']['confirm']) {
    bitcache_delete_repository($form_state['values']['name']);
    drupal_set_message(t('The repository has been deleted.'));

    $form_state['redirect'] = 'admin/settings/bitcache/repositories';
  }
}
