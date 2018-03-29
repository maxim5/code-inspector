<?php

/*------------------------------------------------------------------------------------------------*\
  Copyright (C) 2010 Benjamin Keen
  http://www.opentranslate.org
\*------------------------------------------------------------------------------------------------*/

/*------------------------------------------------------------------------------------------------*\
  VERSIONS - functions in brief

	  ot_get_version_data
    ot_get_data
    ot_get_data_version
    ot_get_multiple_data
		ot_add_data
    ot_update_data
    ot_update_export_only_data
    ot_delete_data
    ot_search_data
    	_ot_search_data_base_version
    	_ot_search_data_child_version
    ot_sort_category_data

\*------------------------------------------------------------------------------------------------*/


/**
 * Retrieves all data for project version, ordered by category then data.
 */
function ot_get_version_data($version_id)
{
	$query = mysql_query("
	  SELECT d.*, pvc.category_name
    FROM   tr_data d, tr_project_categories pvc
		WHERE  d.version_id = $version_id AND
		       d.category_id = pvc.category_id
		ORDER BY pvc.category_order, d.data_category_order
      ") or die(mysql_error());

  $result = array();
  while ($row = mysql_fetch_assoc($query))
    $result[] = $row;

  return $result;
}


/**
 * Retrieves a single piece of data.
 */
function ot_get_data($data_id)
{
	$query = mysql_query("
	  SELECT *
    FROM   tr_data
		WHERE  data_id = $data_id
      ") or die(mysql_error());

  return mysql_fetch_assoc($query);
}


/**
 * Returns the version ID that a piece of data was first defined in.
 */
function ot_get_data_version($data_id)
{
	$query = mysql_query("
	  SELECT version_id
    FROM   tr_data
		WHERE  data_id = $data_id
      ") or die(mysql_error());

  $result = mysql_fetch_assoc($query);
  return $result["version_id"];
}


/**
 * Returns a query of all the data ids specified in the parameter.
 */
function ot_get_multiple_data($data_ids)
{
  if (empty($data_ids))
    return array();

  $where_clause_arr = array();
  foreach ($data_ids as $data_id)
    $where_clause_arr[] = "data_id = $data_id";

  $where_clause = "WHERE " . join(" OR ", $where_clause_arr);
	$query = mysql_query("
	  SELECT *
    FROM   tr_data
		$where_clause
      ") or ot_handle_error(mysql_error());

  return $query;
}


/**
 * Returns all data for a project version, ordered by data order (ASC), then creation date
 * (newest first).
 */
function ot_get_category_data($category_id, $version_id)
{
	// find the base version
	$base_version_id = ot_get_base_version($version_id);

	if ($base_version_id == $version_id)
	{
		$query = mysql_query("
		  SELECT d.*
	    FROM   tr_data d
			WHERE  version_id = $version_id
	    AND    category_id = $category_id
			ORDER BY d.data_category_order ASC, d.creation_date DESC
	      ") or die(mysql_error());

	}
	else
	{
	  $version_chain = ot_get_parent_versions($version_id);
	  $version_chain_str = join(", ", $version_chain);
	  array_shift($version_chain);  // remove the base version
	  $version_chain_str_omit_base_version = join(", ", $version_chain);

		$query = mysql_query("
		  SELECT d.*
		  FROM   tr_data d
		  	INNER JOIN tr_project_categories pvc ON d.category_id = pvc.category_id
		  WHERE  d.version_id IN ($version_chain_str) AND
		         d.category_id = $category_id AND
		  			 d.data_id NOT IN (SELECT data_id FROM tr_data_version_changes WHERE version_id IN ($version_chain_str_omit_base_version)) AND
		  			 d.data_id NOT IN (SELECT data_id FROM tr_data_version_deletions WHERE version_id IN ($version_chain_str_omit_base_version))
			ORDER BY d.data_category_order ASC, d.creation_date DESC
		    ");
	}

  $infohash = array();
	while ($result = mysql_fetch_assoc($query))
	 $infohash[] = $result;

  return $infohash;
}


/**
 * Adds a new data snippet to the database for a particular project + category.
 *
 * @param integer $version_id
 * @param array $info
 */
function ot_add_data($version_id, $info)
{
  $account_id = $_SESSION["ot"]["account_id"];

  $info = ot_clean_hash($info);

  $use_html_editor = $info["use_html_editor"];

  // TODO WTF?!
  $data        = htmlspecialchars($info["data"]); // this only escapes DOUBLE quotes, not single.
  $data        = preg_replace("/'/", "&#39;", $data);
  $data_label  = addslashes($info["data_label"]);

  $category_id     = $info["category_id"];
  $insert_position = $info["insert_position"];
  $comments_for_translators = addslashes($info["comments_for_translators"]);
  $version_id = $info["version_id"];

  // get num words in data to get an idea of the data size
  $data_size = count(explode(" ", $data));
  $category_data = ot_get_category_data($category_id, $version_id);
  $re_sort_category_id = false;


  switch ($insert_position)
  {
    case "start":
      $data_category_order = 1;
      $re_sort_category_id = true;
      break;

    case "end":
      $data_category_order = count($category_data) + 1;
      break;

    case "after":
      $insert_position_after_data_id = $info["insert_position_after"];
      $order = 0;
      foreach ($category_data as $info)
      {
        if ($info["data_id"] == $insert_position_after_data_id)
        {
          $order = $info["data_category_order"];
          break;
        }
      }
      $data_category_order = $order + 1;
      $re_sort_category_id = true;
      break;
  }

  $now = ot_get_current_datetime();

  mysql_query("
    INSERT INTO tr_data (category_id, data_category_order, version_id,
      data_label, data, data_size, comments_for_translators, creation_date, last_modified,
      created_by_account_id, use_html_editor)
    VALUES ('$category_id', '$data_category_order', '$version_id', '$data_label', '$data',
      $data_size, '$comments_for_translators', '$now', '$now', '$account_id', '$use_html_editor')
      ")
      or ot_handle_error(mysql_error());

  $new_data_id = mysql_insert_id();

  // if required, re-sort all the data in this category (var name "re_sort_category_id"?! Was I stoned?)
  if ($re_sort_category_id)
    ot_sort_category_data($category_id, $version_id, "current");

  return $new_data_id;
}


/**
 * Updates a data snippet. Called by administrators & project managers.
 *
 * This function also DETACHES a data item and associates it with a new version. It does
 * this whenever a user selects a different version than the one the data is currently associated
 * with.
 *
 * @param integer $data_id
 * @param hash $info
 */
function ot_update_data($data_id, $info)
{
  $account_id = $_SESSION["ot"]["account_id"];

  $info = ot_clean_hash($info);

	$category_id = $info["category_id"];
  $data_id     = $info["data_id"];
  $data_label  = $info["data_label"];
  $data        = $info["data"];
  $use_html_editor = $info["use_html_editor"];
  $comments_for_translators = $info["comments_for_translators"];

  // get num words in data to get an idea of the data size
  $data_size = count(explode(" ", $data));
  $now = ot_get_current_datetime();
  $affected_version_id = $info["affected_version_id"];

  // find out which version this data is currently associated with
  $data_info = ot_get_data($data_id);

  // compare with the $affected_version_id. This determines whether or not the user is updating the
  // original record, or detaching this change from the previous version
  $version_info = ot_get_project_version($affected_version_id);

  if ($data_info["version_id"] == $affected_version_id)
  {
	  $result = mysql_query("
	    UPDATE tr_data
	    SET    comments_for_translators = '$comments_for_translators',
	           data_label = '$data_label',
	           data = '$data',
	           data_size = $data_size,
	           category_id = '$category_id',
	           last_modified = '$now',
	           use_html_editor = '$use_html_editor'
	    WHERE  data_id = $data_id
	      ");
	}

  // the user wants to detach this from the original version. No problem! Just create a new
  else
  {
  	$data_category_order = $data_info["data_category_order"];

  	mysql_query("
	    INSERT INTO tr_data (category_id, data_category_order, version_id,
	      data_label, data, data_size, comments_for_translators, creation_date, last_modified,
	      created_by_account_id, use_html_editor)
	    VALUES ('$category_id', '$data_category_order', '$affected_version_id', '$data_label', '$data',
	      $data_size, '$comments_for_translators', '$now', '$now', '$account_id', '$use_html_editor')
	      ")
	      or ot_handle_error(mysql_error());

	  $new_data_id = mysql_insert_id();
		$_SESSION["ot"]["version_id"] = $affected_version_id;

    header("location: {$_SERVER["REQUEST_URI"]}?data_id=$new_data_id&detached=1");
    exit;
  }


  if ($result)
    return array(true, "The data item has been updated.");
  else
    return array(false, "The data item has been updated.");
}


/**
 * Updates a data snippet from a "special"
 */
function ot_update_export_only_data($data_id, $info)
{
  $info = ot_clean_hash($info);
  $account_id = $_SESSION["ot"]["account_id"];
  $now = ot_get_current_datetime();

  while (list($field_name, $value) = each($info))
  {
    // add any new "translations" (export only data items)
    if (preg_match("/^special_add_language_(\d+)/", $field_name, $matches))
    {
      $language_id = $matches[1];

      if (empty($value))
        continue;

      mysql_query("
        INSERT INTO tr_data_translations (data_id, translator_id, language_id, translation, creation_date, translation_status)
        VALUES ($data_id, $account_id, $language_id, '$value', '$now', 'completed')
          ") or die(mysql_error());
    }

    // add any new "translations" (export only data items)
    if (preg_match("/^special_update_translation_(\d+)/", $field_name, $matches))
    {
      $translation_id = $matches[1];

      if (empty($value))
        continue;

      mysql_query("
        UPDATE tr_data_translations
        SET    data_id = $data_id,
               translation = '$value',
               creation_date = '$now'
        WHERE  translation_id = $translation_id
          ") or die(mysql_error());
    }
  }

  return array(true, "The data item has been updated.");
}


/**
 * Deletes a particular data snippet and all translations of it. This function does two things,
 * depending on where it's being deleted from.
 *   Case 1: from a base version
 *     - delete the data item and all translations made of it
 *   Case 2: from a child version
 *     - mark the data item as deleted in the version and all children of this version, but leave the
 *       translations intact
 */
function ot_delete_data($data_id, $info)
{
	$delete_base_version = true;

	// find out if the version being passed is actually just the base version
	if (isset($info["delete_version_id"]))
	  $version_id = $info["delete_version_id"];
	else
    $version_id = $_SESSION["ot"]["version_id"];

	$old_data = ot_get_data($data_id);

	// Case 1: we're deleting from the same version that it was first defined in
	if ($old_data["version_id"] == $version_id)
	{
	  $translations = ot_get_data_translations($data_id);
	  $category_id  = $old_data["category_id"];
	  $version_id   = $old_data["version_id"];

	  // delete all histories for the translations
	  foreach ($translations as $translation_info)
	    ot_delete_translation($translation_info["translation_id"]);

	  // delete the actual data
	  mysql_query("DELETE FROM tr_data WHERE data_id = $data_id");

	  // delete the actual translations
	  mysql_query("DELETE FROM tr_data_translations WHERE data_id = $data_id");

	  // if any (child) versions had explicitly deleted this data, remove up the redundant old data
	  mysql_query("DELETE FROM tr_data_version_deletions WHERE data_id = $data_id");

	  // if any (child) versions had overridden this data, remove the redundant data
	  mysql_query("DELETE FROM tr_data_version_changes WHERE data_id = $data_id");

	  // fix the ordering of the data in this category
	  ot_sort_category_data($category_id, $version_id, "current");
	}

	// Case 1: deleting from version in which it wasn't first defined. No problem!
	else
	{
		// Just add the delete record to the tr_data_version_deletions table
    mysql_query("
      INSERT INTO tr_data_version_deletions (version_id, data_id)
      VALUES ($version_id, $data_id)
        ");
	}
}


/**
 * Creates and returns a search for any version data set, and any subset of its columns, returning results in any
 * column order and for any single page subset (or all pages). This function is used for all account types to
 * return the main data sets.
 *
 * In order to understand this function, you need to understand two things: (1) Open Translate's inheritance model
 * for the source data and (2) the database structure.
 *
 *   1. When a project is first created, it gets a default version. This is known as a "base version",
 *   2. All content that's added to that version to be translated is added to the tr_data table.
 *   3. Projects may have any number of versions. Versions may contain totally new content or inherit their data
 *      from a previous version (e.g. a version 1.1 which makes a few change to the data to be translated).
 *   4. (a) for versions with a totally new data set, they ALSO become a "base version" and their data is stored
 *          in tr_data
 *      (b) for versions that inherit from a previous version, their data is stored in tr_data_version_changes.
 *   5. (recap!) tr_data will always store base version data; tr_data_version_changes will always store version data
 *      that inherit from another version.
 *   6. A version may inherit from a version which inherits from a version, etc. It can be a long chain of version
 *      inheritance. But ALL of those versions will have their data stored in tr_data_version_changes.
 *   7. The two tables (tr_data and tr_data_version_changes) are superficially similar, but actually very different.
 *      When a version inherits from a previous version, it DOESN'T get its own copy of the data. So a newly created
 *      version based on a previous version will not have ANY data stored in tr_data_version_changes. That table
 *      stores the *flattened* changes for all versions made to it and any previous versions - with the exception of
 *      the base version.
 *
 *      For example, imagine we create a new version which inherits like so: 1.3 -> 1.2 -> 1.1 -> 1.0.
 *      Assuming changes (updates, deletions and additions) were made to 1.1 and 1.2, then 1.3 will contain the TOTAL
 *      list of all changes made to those two versions as well as its own.
 *
 *      N.B. I realize this will result in some database redundancy, but I felt that the advantage of being able
 *      to do a single query to search an entire version QUICKLY and SIMPLY outweighed this consideration.
 *
 *      N.B.B - TODO - try just referencing the original data_id (or ID) of the original record being overridden. Perhaps
 *      the query can be modified to return that info easily? That would be sweet.

 *
 * Known issues:
 * - This code doesn't take into consideration the original version being "not visible" or "not translatable".
 */
function ot_search_data($version_id, $results_per_page, $page_num, $order, $language_id,
  $category_id = "", $g_data_size = "", $search_criteria = array())
{
	// find the base version
	$base_version_id = ot_get_base_version($version_id);

	// if we're just searching a base version, sweet! The query is nice and simple.
	$return_hash = array();
	if ($base_version_id == $version_id)
	{
		$return_hash = _ot_search_data_base_version($version_id, $results_per_page, $page_num, $order, $language_id, $category_id,
        $g_data_size, $search_criteria);
	}
	else
	{
		$return_hash = _ot_search_data_child_version($version_id, $results_per_page, $page_num, $order, $language_id, $category_id,
		    $g_data_size, $search_criteria);
	}

  return $return_hash;
}



/**
 * Searches data in a base version.
 *
 * @param integer $version_id
 * @param integer $results_per_page
 * @param integer $page_num
 * @param string $order
 * @param array $languages
 * @param integer $category_id
 * @param string $g_data_size
 * @param array $search_criteria
 * @return array
 */
function _ot_search_data_base_version($version_id, $results_per_page, $page_num, $order, $language_id,
  $category_id, $g_data_size, $search_criteria)
{
	global $g_PHRASE_SIZE, $g_SENTENCE_SIZE, $g_PARAGRAPH_SIZE;

  // sorting by column, format: col_x-desc / col_y-asc
  list($column, $direction) = split("-", $order);

  if ($column == "data_category_order")
	  $order_by = "pvc.category_order $direction, d.data_category_order $direction ";
  else
	  $order_by = "d.$column $direction";

	// if required, return a specific category only
	$extra_where_clause = "";
	if (!empty($category_id) && $category_id != "all")
	  $extra_where_clause = "AND d.category_id = $category_id ";

  // if performing a search, add those particular limitations
  if (!empty($search_criteria))
  {
    if (isset($search_criteria["php_label"]))
      $extra_where_clause .= "AND d.data_label LIKE '%{$search_criteria['php_label']}%' ";
    if (isset($search_criteria["data_string"]))
      $extra_where_clause .= "AND d.data LIKE '%{$search_criteria['data_string']}%' ";
  }

  // determine the LIMIT clause
  $limit_clause = "";
  if ($results_per_page != "all")
  {
		if (empty($page_num))
			$page_num = 1;
		$first_item = ($page_num - 1) * $results_per_page;

		$limit_clause = "LIMIT $first_item, $results_per_page";
  }

  // if required, limit the size
  $size_limit_clause = "";
  switch ($g_data_size)
  {
    // word
    case "words":
      $size_limit_clause = "AND d.data_size = 1";
      break;

    // phrase
    case "phrases":
      $size_limit_clause = "AND d.data_size > 1 AND d.data_size <= $g_PHRASE_SIZE";
      break;

    // sentence
    case "sentences":
      $size_limit_clause = "AND d.data_size > $g_PHRASE_SIZE AND d.data_size <= $g_SENTENCE_SIZE";
      break;

    // paragraph
    case "paragraphs":
      $size_limit_clause = "AND d.data_size > $g_SENTENCE_SIZE AND d.data_size <= $g_PARAGRAPH_SIZE";
      break;

    // document
    case "documents":
      $size_limit_clause = "AND d.data_size > $g_PARAGRAPH_SIZE";
      break;

    // the default setting is blank. Do nothing: all data sizes are returned
    default:
    	 break;
  }

  $full_query = "
    SELECT *, d.data_id as curr_data_id
    FROM   tr_data d
      LEFT JOIN tr_data_translations dt ON d.data_id = dt.data_id AND language_id = $language_id
      INNER JOIN tr_project_categories pvc ON d.category_id = pvc.category_id
    WHERE  d.version_id = $version_id
  	    	 $extra_where_clause
  		     $size_limit_clause
    ORDER BY $order_by
           $limit_clause
               ";
  $count_query = "
      SELECT count(*) as num_results
      FROM   tr_data d
        INNER JOIN tr_project_categories pvc ON d.category_id = pvc.category_id
      WHERE  d.version_id = $version_id
			       $extra_where_clause
			       $size_limit_clause
                 ";

  $search_query = mysql_query($full_query)
    or die(__FUNCTION__ . ", failed query: " . mysql_error());

  $query = mysql_query($count_query);
	$result = mysql_fetch_assoc($query);
  $num_results = $result["num_results"];

  $return_hash = array(
    "search_query" => $search_query,
    "num_results"  => $num_results
      );

  return $return_hash;
}



function _ot_search_data_child_version($version_id, $results_per_page, $page_num, $order, $language_id,
  $category_id, $g_data_size, $search_criteria)
{
	global $g_PHRASE_SIZE, $g_SENTENCE_SIZE, $g_PARAGRAPH_SIZE;

  // sorting by column, format: col_x-desc / col_y-asc
  list($column, $direction) = split("-", $order);

  if ($column == "data_category_order")
	  $order_by = "pvc.category_order $direction, d.data_category_order $direction ";
  else
	  $order_by = "d.$column $direction";

	// if required, return a specific category only
	$where_clauses = array();
	if (!empty($category_id) && $category_id != "all")
	  $where_clauses[] = "d.category_id = $category_id";

  // if performing a search, add those particular limitations
  if (!empty($search_criteria))
  {
    if (isset($search_criteria["php_label"]))
      $where_clauses[] = "d.data_label LIKE '%{$search_criteria['php_label']}%'";
    if (isset($search_criteria["data_string"]))
      $where_clauses[] = "d.data LIKE '%{$search_criteria['data_string']}%'";
  }

  // if required, limit the size
  switch ($g_data_size)
  {
    case "words":
      $where_clauses[] = "d.data_size = 1";
      break;
    case "phrases":
      $where_clauses[] = "d.data_size > 1 AND d.data_size <= $g_PHRASE_SIZE";
      break;
    case "sentences":
      $where_clauses[] = "d.data_size > $g_PHRASE_SIZE AND d.data_size <= $g_SENTENCE_SIZE";
      break;
    case "paragraphs":
      $where_clauses[] = "d.data_size > $g_SENTENCE_SIZE AND d.data_size <= $g_PARAGRAPH_SIZE";
      break;
    case "documents":
      $where_clauses[] = "d.data_size > $g_PARAGRAPH_SIZE";
      break;

    default:
    	 break;
  }

  // determine the LIMIT clause
  $limit_clause = "";
  if ($results_per_page != "all")
  {
		if (empty($page_num))
			$page_num = 1;
		$first_item = ($page_num - 1) * $results_per_page;

		$limit_clause = "LIMIT $first_item, $results_per_page";
  }

  $where_clause = (empty($where_clauses)) ? "" : "AND " . join(" AND ", $where_clauses);


  $version_chain = ot_get_parent_versions($version_id);
  $version_chain_str = join(", ", $version_chain);

  array_shift($version_chain);  // remove the base version
  $version_chain_str_omit_base_version = join(", ", $version_chain);

  // boy this took time to figure out. This query is smart enough to only return the appropriate
  // results for this particular version. It takes into account deletions and updates made to any versions
  // along the version inheritance chain
  // N.B. we include the tr_data_translations table in the search to get a little info about the stat
	$full_query = "
		  SELECT *, d.data_id as curr_data_id
		  FROM   tr_data d
        LEFT JOIN tr_data_translations dt ON d.data_id = dt.data_id AND language_id = $language_id
		  	INNER JOIN tr_project_categories pvc ON d.category_id = pvc.category_id
		  WHERE  d.version_id IN ($version_chain_str) AND
		  			 d.data_id NOT IN (SELECT data_id FROM tr_data_version_changes WHERE version_id IN ($version_chain_str_omit_base_version)) AND
		  			 d.data_id NOT IN (SELECT data_id FROM tr_data_version_deletions WHERE version_id IN ($version_chain_str_omit_base_version))
		  			 $where_clause
		  			 $limit_clause
	      ";

	$count_query = "
		  SELECT count(*) as num_results
		  FROM   tr_data d
		  		INNER JOIN tr_project_categories pvc ON d.category_id = pvc.category_id
		  WHERE  d.version_id IN ($version_chain_str) AND
		  			 d.data_id NOT IN (SELECT data_id FROM tr_data_version_changes WHERE version_id IN ($version_chain_str_omit_base_version)) AND
		  			 d.data_id NOT IN (SELECT data_id FROM tr_data_version_deletions WHERE version_id IN ($version_chain_str_omit_base_version))
		  			 $where_clause
		    ";

  $search_query = mysql_query($full_query)
    or die(__FUNCTION__ . ", failed query: " . mysql_error());

  $query = mysql_query($count_query);
	$result = mysql_fetch_assoc($query);
  $num_results = $result["num_results"];

  $return_hash = array(
    "search_query" => $search_query,
    "num_results"  => $num_results
      );

  return $return_hash;
}


/**
 * Sorts all data in a category.
 *
 * @param integer $category_id
 * @param integer $version_id
 * @param integer $sort_order
 *                    "current" sorts by existing order first, creation_date (newest first)
 *                         of the data second. This second condition is useful after an insertion is done.
 *                     "php_label" - alphabetically
 *                     "data"      - alphabetically
 */
function ot_sort_category_data($category_id, $version_id, $sort_order)
{
  // this returns the category data ordered by data_category_order first, then creation data
  $category_data = ot_get_category_data($category_id, $version_id);

  switch ($sort_order)
  {
    case "current":
      $count = 1;
      foreach ($category_data as $info)
      {
        $data_id = $info["data_id"];
        mysql_query("UPDATE tr_data SET data_category_order = $count WHERE data_id = $data_id");
        $count++;
      }
      break;
  }
}


/**
 * Returns all versions that a particular piece of data is being shared by. This is passed a reference
 * version ID so that it can figure out the relevant chain of version inheritance.
 *
 * @param integer $data_id
 * @param integer $version_id
 * @return array of $version_ids
 */
function ot_display_data_usage_version_tree($version_id, $data_id)
{
	// this gets the entire tree structure for this version
  $version_tree = ot_get_version_tree($version_id);

  // find out in which version the data was first defined
  $data_version_id = ot_get_data_version($data_id);

  // now determine the CHILD versions that may use this data (any data that's defined in a child
  // is NOT defined for the parent).
  $tree = ot_get_tree_fragment($version_tree, $data_version_id);

  // now display the tree. We pass the $data ID since any child versions may have deleted the data from it.
  // this will ignore those versions and any of their children
  ot_display_version_tree($tree, $data_id, $version_id);
}


/**
 * Returns a flattened list of version IDs for a particular piece of data.
 *
 * @param integer $version_id
 * @param integer $data_id
 * @return array
 */
function ot_get_data_usage_versions($version_id, $data_id)
{
  $version_tree = ot_get_version_tree($version_id);

  // find out in which version the data was first defined
  $data_version_id = ot_get_data_version($data_id);

  // now determine the tree of versions that may use this data
  $tree = ot_get_tree_fragment($version_tree, $data_version_id);

  // flatten the version IDs and figure out which use it!
  $keys = ot_multiarray_keys($tree);

  $affected_version_ids = array();
  foreach ($keys as $version_id)
  {
	if (!ot_data_deleted_in_version($data_id, $version_id))
    	$affected_version_ids[] = $version_id;
  }

  return $affected_version_ids;
}


/**
 * Displays an HTML tree of versions that are relevant for a particular data ID. This is used
 * on the Edit Data page to let the admin/project manager
 *
 * @param unknown_type $tree
 * @return unknown_type
 */
function ot_display_version_tree($tree, $data_id, $current_version_id)
{
	echo "<ul>";
  $version_ids = array_keys($tree);
  foreach ($version_ids as $version_id)
  {
  	if (ot_data_deleted_in_version($data_id, $version_id))
  	  continue;

  	$version_name    = ot_get_version_name($version_id);
    $highlight_class = ($current_version_id == $version_id) ? "curr_version" : "";
    $checked         = ($version_id == $current_version_id) ? "checked" : "";

  	echo <<< EOF
  	<li>
<input type="radio" name="affected_version_id" value="$version_id" id="affected_version_id_{$version_id}" $checked />
  <label for="affected_version_id_$version_id" class="$highlight_class">$version_name</label>
EOF;

  	if (!empty($tree[$version_id]))
  	{
  	  ot_display_version_tree($tree[$version_id], $data_id, $current_version_id);
  	}

  	echo "</li>";
  }
	echo "</ul>";
}


/**
 * A helper function that returns a boolean indicating whether a piece of data was deleted from
 * a particular version. This is NOT smart - it doesn't check to see if it was deleted in a parent
 * version anywhere up the version tree.
 *
 * @return boolean true = has been deleted, false otherwise
 */
function ot_data_deleted_in_version($data_id, $version_id)
{
  $query = mysql_query("
    SELECT *
    FROM   tr_data_version_deletions
    WHERE  version_id = $version_id AND
           data_id    = $data_id
      ") or die(mysql_error());
	$result = mysql_fetch_assoc($query);

	return !empty($result);
}


/**
 * Returns an array of versions that have deleted a data item.
 *
 * @param integer $data_id
 * @return array
 */
function ot_get_versions_that_deleted_data($data_id)
{
  $query = mysql_query("
    SELECT version_id
    FROM   tr_data_version_deletions
    WHERE  data_id = $data_id
  ");

  $info = array();
  while ($row = mysql_fetch_assoc($query))
    $info[] = $row["version_id"];

  return $info;
}


/**
 * Helper function to return the total number of data items in a version.
 *
 * @param integer $version_id
 * @return integer
 */
function ot_get_num_version_data($version_id, $omit_export_only = true)
{
	$base_version_id = ot_get_base_version($version_id);

	$export_only_clause = ($omit_export_only) ? "AND pc.export_only = 'no'" : "";

	// if we're just searching a base version, sweet! The query is nice and simple.
	if ($base_version_id == $version_id)
	{
		$count_query = mysql_query("
	    SELECT count(*) as num_data
	    FROM   tr_data d, tr_project_categories pc
	    WHERE  d.version_id = $version_id AND
	           d.category_id = pc.category_id
	           $export_only_clause
	             ");
	}
	else
	{
	  $version_chain = ot_get_parent_versions($version_id);
	  $version_chain_str = join(", ", $version_chain);

	  array_shift($version_chain);  // remove the base version
	  $version_chain_str_omit_base_version = join(", ", $version_chain);

	  // TODO
	  // 		  removed:		INNER JOIN tr_project_categories pvc ON d.category_id = pvc.category_id
    $count_query = mysql_query("
		  SELECT count(*) as num_data
		  FROM   tr_data d, tr_project_categories pc
		  WHERE  d.version_id IN ($version_chain_str) AND
		         d.category_id = pc.category_id AND
		  			 d.data_id NOT IN (SELECT data_id FROM tr_data_version_changes WHERE version_id IN ($version_chain_str_omit_base_version)) AND
		  			 d.data_id NOT IN (SELECT data_id FROM tr_data_version_deletions WHERE version_id IN ($version_chain_str_omit_base_version))
		  			 $export_only_clause
        ");
	}

  $result = mysql_fetch_assoc($count_query);

  return $result["num_data"];
}