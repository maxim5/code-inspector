/* data.cpp - Network data code, describes data formatting when transferring
 *			  over network.
 *
 * Copyright (C) 2011 Craig Sturdy
 *
 * Email: craig <at> sturd <dot> co <dot> uk
 * Example Usage: http://sturd.co.uk/
 *
 * data.cpp is the legal property of its developer.
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.

 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.

 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */

#include <string.h>
#include <iostream>

#include "data.h"

net_data::net_data()
{
	packet_status = PACKET_STAT_CONN_BLN;
}

net_data::net_data( short status )
{
	set_status( status );
}

net_data::net_data( player_data *data, const short &client_id )
{	// Initialise game data packet
	packet_status = PACKET_STAT_CONN_DAT;
	char *temp_data = ( char * )data;
	for( int i = 0; i < PLAYER_DATA_SIZE; ++i )
		packet_content[ i ] = temp_data[ i ];
		
	client_id_ = client_id;
}

/*
	operator == - Allows quick boolean statement in checking
				  the intent of the received packet.
 */
bool net_data::operator==( const short &status )const
{
	return ( packet_status == status );
}

/*
	set_status() - Allow externals to change the status of
				   the packet in order to request different
				   material from the client/server
 */
void net_data::set_status( short status )
{
	packet_status = status;
}

short net_data::get_client_id()
{
	return client_id_;
}

player_data *net_data::get_player_data()
{
	return ( player_data * )&packet_content;
}
