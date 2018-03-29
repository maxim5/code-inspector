<?php

/**
 * @file
 * Contains database lookup functions for the HFCC Catalog module.
 *
 * @see hfcc_catalog.module
 */

/**
 * Retrieve a list of catalog_course nodes for field options.
 */
function hfcc_catalog_catalog_course_options() {
  $options = &drupal_static(__FUNCTION__);
  if (!isset($options)) {
    if ($cache = cache_get('hfcc_catalog_catalog_courses')) {
      $options = $cache->data;
    }
    else {
      $courses = db_query("SELECT nid, title FROM {node} WHERE (type = 'catalog_course' AND status = 1) ORDER BY title ASC")->fetchAll();
      $options = array('' => t('--select a course--'));
      foreach ($courses as $node) {
        $options[$node->nid] = $node->title;
      }
      cache_set('hfcc_catalog_catalog_courses', $options, 'cache', CACHE_TEMPORARY);
    }
  }
  return $options;
}

/**
 * Retrieve a list of catalog_course nodes for program sequence options.
 */
function hfcc_catalog_program_sequence_options() {
  $options = &drupal_static(__FUNCTION__);
  if (!isset($options)) {
    if ($cache = cache_get('hfcc_program_sequence_options')) {
      $options = $cache->data;
    }
    else {
      $query = "SELECT n.nid, s.field_crs_subj_value, m.field_crs_num_value FROM {node} n ";
      $query .= "LEFT JOIN {field_data_field_crs_subj} s ON (n.nid = s.entity_id) AND (n.vid = s.revision_id) AND (s.entity_type = 'node') AND (s.bundle = 'catalog_course') ";
      $query .= "LEFT JOIN {field_data_field_crs_num} m ON (n.nid = m.entity_id) AND (n.vid = m.revision_id) AND (m.entity_type = 'node') AND (m.bundle = 'catalog_course') ";
      $query .= "WHERE (type = 'catalog_course' AND status = 1) ORDER BY n.title ASC";
      $courses = db_query($query)->fetchAll();
      $options = array('' => t('select'));
      foreach ($courses as $node) {
        $options[$node->nid] = $node->field_crs_subj_value . ' ' . $node->field_crs_num_value;
      }
      cache_set('hfcc_program_sequence_options', $options, 'cache', CACHE_TEMPORARY);
    }
  }
  return $options;
}

/**
 * Look up course title by course number.
 */
function hfcc_catalog_lookup_seq_course_by_number($course_number) {

  $cid = 'hfcc_catalog_lookup_seq_course_' . $course_number;
  if ($cache = cache_get($cid)) {
    return $cache->data;
  }
  else {
    preg_match("/^([A-Z]+) ?([0-9]+)$/", drupal_strtoupper($course_number), $matches);

    if (!empty($matches[2])) {
      $subj = $matches[1];
      $num = $matches[2];
    }
    else {
      $subj = NULL; $num = NULL;
    }
    if (!empty($subj) && !empty($num)) {
      $query = db_select('node', 'n');
      $query->join('field_data_field_crs_subj', 's', "(n.nid = s.entity_id) AND (n.vid = s.revision_id) AND (s.entity_type = 'node') AND (s.bundle = 'catalog_course')");
      $query->join('field_data_field_crs_num', 'm', "(n.nid = m.entity_id) AND (n.vid = m.revision_id) AND (m.entity_type = 'node') AND (m.bundle = 'catalog_course')");
      $query->join('field_data_field_crs_title', 't', "(n.nid = t.entity_id) AND (n.vid = t.revision_id) AND (t.entity_type = 'node') AND (t.bundle = 'catalog_course')");
      $query->leftjoin('field_data_field_crs_prq', 'q', "(n.nid = q.entity_id) AND (n.vid = q.revision_id) AND (q.entity_type = 'node') AND (q.bundle = 'catalog_course')");
      $query->leftjoin('field_data_field_crs_credit_hours', 'c', "(n.nid = c.entity_id) AND (n.vid = c.revision_id) AND (c.entity_type = 'node') AND (c.bundle = 'catalog_course')");
      $query->leftjoin('field_data_field_course_master', 'i', "(n.nid = i.entity_id) AND (n.vid = i.revision_id) AND (i.entity_type = 'node') AND (i.bundle = 'catalog_course')");
      // We need the node entry for course_master to get the vid for this. Do we really need it?
      $query->leftjoin('field_data_field_crs_student_hours', 'h', "(i.field_course_master_target_id = h.entity_id) AND (h.entity_type = 'node') AND (h.bundle = 'course_master')");
      $query->addField('n', 'nid', 'nid');
      $query->addField('t', 'field_crs_title_value', 'title');
      $query->addField('q', 'field_crs_prq_value', 'prerequisites');
      $query->addExpression('ROUND(field_crs_credit_hours_value)', 'credit_hours');
      $query->addExpression('ROUND((field_crs_student_hours_value - 2)/15)', 'contact_hours');
      $query->condition('s.field_crs_subj_value', $subj, '=');
      $query->condition('m.field_crs_num_value', $num, '=');
      if ($course = $query->execute()->fetchAssoc()) {
        // dpm($course, 'course');
        cache_set($cid, $course, 'cache', CACHE_TEMPORARY);
        return $course;
      }
    }
  }
  // Return empty values if lookup fails.
  return array(
    'nid' => NULL,
    'title' => NULL,
    'prerequisites' => NULL,
    'credit_hours' => NULL,
    'contact_hours' => NULL,
  );
}

/**
 * Build course links for custom program field types.
 */
function hfcc_catalog_program_course_link($nid, $type) {
  $markup = NULL;

  $query = db_select('node', 'n');
  $query->fields('n', array('title'));
  $query->condition('nid', $nid, '=');
  $query->join('field_data_field_crs_subj', 's', "(n.nid = s.entity_id) AND (n.vid = s.revision_id) AND (s.entity_type = 'node') AND (s.bundle = 'catalog_course')");
  $query->join('field_data_field_crs_num', 'm', "(n.nid = m.entity_id) AND (n.vid = m.revision_id) AND (m.entity_type = 'node') AND (m.bundle = 'catalog_course')");
  $query->addField('s', 'field_crs_subj_value', 'subject');
  $query->addField('m', 'field_crs_num_value', 'number');
  $node = $query->execute()->fetchAssoc();

  $local_link = 'node/' . $nid;
  $export_link = drupal_strtolower('catalog/courses/' . $node['subject'] . '-' . $node['number']);
  $course_num = drupal_strtoupper($node['subject'] . ' ' . $node['number']);

  if (!empty($node)) {
    switch ($type) {
      case 'program_course_formatter':
        $markup = l($node['title'], $local_link);
        break;
      case 'program_course_export':
        $markup = l($node['title'], $export_link);
        break;
      case 'program_sequence_formatter':
        $markup = l($course_num, $local_link);
        break;
      case 'program_sequence_export':
        $markup = l($course_num, $export_link);
        break;
    }
  }
  else {
    switch ($type) {
      case 'program_course_formatter':
      case 'program_course_export':
        $markup = t('Error loading specified course.');
        break;
      case 'program_sequence_formatter':
      case 'program_sequence_export':
        $markup = t('ERROR');
        break;
    }
    drupal_set_message(t('Error loading specified course %nid', array('%nid' => $nid)), 'warning');
  }

  return $markup;
}

/**
 * Return an array of Transfer Colleges, sorted by college name for use as a services resource.
 */
function _hfcc_catalog_xfer_colleges() {
  $result = db_query("SELECT id, name FROM {hfcc_catalog_xfer_colleges} ORDER BY name ASC")->fetchAll();
  return $result;
}

/**
 * Return an array of Transfer Colleges, sorted by college name as a services resource.
 */
function hfcc_catalog_xfer_college_options() {
  $result = _hfcc_catalog_xfer_colleges();
  $options = array();
  foreach ($result as $value) {
    $options[$value->id] = $value->name;
  }
  return $options;
}

/**
 * Retrieve boilerplate content to display.
 */
function hfcc_catalog_get_boilerplate_info($section, $degree_type) {
  $items = array();
  $sections = hfcc_catalog_boilerplate_sections();
  $query = new EntityFieldQuery();
  $result = $query
    ->entityCondition('entity_type', 'node')
    ->entityCondition('bundle', 'catalog_boilerplate')
    ->propertyCondition('status', 1)
    ->fieldCondition('field_bp_section', 'value', $section)
    ->fieldCondition('field_degree_type', 'value', $degree_type)
    ->execute();
  if (!empty($result['node'])) {
    $nids = array_keys($result['node']);
    $boilerplate = node_load(reset($nids));
    $language = $boilerplate->language;
    if (!empty($boilerplate->field_bp_text[$language][0])) {
      $items[] = array('#markup' => $boilerplate->field_bp_text[$language][0]['safe_value']);
    }
    if (!empty($boilerplate->field_bp_webpage[$language][0])) {
      $title = !empty($boilerplate->field_bp_webpage[$language][0]['title']) ? $boilerplate->field_bp_webpage[$language][0]['title'] : 'requirements';
      $url = $boilerplate->field_bp_webpage[$language][0]['url'];
      $options = array('attributes' => array('target' => '_blank'));
      $items[] = array(
        '#prefix' => '<p>',
        '#markup' => l(t('Click here to see all of the @title.', array('@title' => $title)), $url, $options),
        '#suffix' => '</p>',
      );
    }
  }
  return $items;
}

/**
 * Return a list of HFCC degree types.
 */
function hfcc_catalog_degree_types() {
  return array(
    'AA' => t('Associate in Arts'),
    'AAS' => t('Associate in Applied Science'),
    'AB' => t('Associate in Business'),
    'AGS' => t('Associate in General Studies'),
    'AS' => t('Associate in Science'),
    'CERT' => t('Certificate of Achievement'),
    'AREA' => t('Area of Study'),
  );
}

/**
 * Return an index of HFCC degree types for services resource.
 */
function _hfcc_catalog_degree_types() {
  return hfcc_catalog_degree_types();
}

/**
 * Return a list of HFCC boilerplate sections.
 */
function hfcc_catalog_boilerplate_sections() {
  return array(
    'RQCOR' => t('Required Core Courses'),
    'RQSUP' => t('Required Support Courses'),
    'DEGSP' => t('Degree-Specific Requirements'),
    'ELECT' => t('Elective Courses'),
    'GENED' => t('General Education'),
    'XFOPT' => t('Transfer Options/Requirements'),
    'DSCLM' => t('General Disclaimer'),
  );
}

/**
 * Define program sequence term label options.
 */
function hfcc_catalog_program_sequence_terms() {
  return array(
    'FA1' => 'Fall 1',
    'WI1' => 'Winter 1',
    'SP1' => 'Spring 1',
    'SU1' => 'Summer 1',
    'SS1' => 'Spring/Summer 1',
    'FA2' => 'Fall 2',
    'WI2' => 'Winter 2',
    'SP2' => 'Spring 2',
    'SU2' => 'Summer 2',
    'SS2' => 'Spring/Summer 2',
    'FA3' => 'Fall 3',
    'WI3' => 'Winter 3',
    'SP3' => 'Spring 3',
    'SU3' => 'Summer 3',
    'SS3' => 'Spring/Summer 3',
  );
}
