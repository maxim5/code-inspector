data.progressiveleadership.org
<?php
/* Proof of concept code
$base_folder =  dirname( dirname( __FILE__ ));
require_once( $base_folder . '/start.php5');
require_once( APP_ROOT . '/lib/dabble_parser.php5');
require_once( APP_ROOT . '/load_dia.php5');

$raw_json = file_get_contents( $config['dabble_sources']['ohio'] );
$parsed_data = DabbleParser::parse( $raw_json );

$dia = dia_connection( $config['democracy_in_action']);
$dia_entries = array_map( 'map_dabble_to_dia', $parsed_data );
for( $n=100;$n<105;$n++) {
    #$dia_entries[$n] = map_dabble_to_dia( $parsed_data[$n+100]);
    if( !isset( $dia_entries[$n]['Email'])) continue;

    $group_keys = $dia_entries[$n]['groups_KEY'];
    unset( $dia_entries[$n]['groups_KEY'] );
    $result = $dia->save( 'supporter', $dia_entries[$n]);
    print "{$dia_entries[$n]['First_Name']} {$dia_entries[$n]['Last_Name'] } $result<br>\n\n";

    if ( !empty( $group_keys )) {
        foreach( $group_keys as $group_key ) {
            print "should add to group " . $group_key . "<br/>\n";
            $dia->save( 'supporter_groups', array( 'supporter_KEY' => $result, 'groups_KEY' => $group_key ));
        }
    }
    print "<br/><br/>\n\n";
}

$has_email = 0;
foreach( $dia_entries as $entry ) {
    if( isset( $entry['Email']) && $entry['Email']) $has_email++;
}


print "<BR><BR>";
print count( $parsed_data );
print " total entries, ";
print $has_email . " with emails";

#print count( $records );
#*/
