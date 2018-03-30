//
//  Data.m
//  teamsSqlite
//
//  Created by Andrea Giannantonio on 09/03/12.
//  Copyright (c) 2012 __MyCompanyName__. All rights reserved.
//

#import "Data.h"

static sqlite3 *database = nil;

@implementation Data

@synthesize lista;


- (id)init {
    self = [super init];
    if (self) {
        // definiamo il percorso del db
        pathDB = [[[NSBundle mainBundle] resourcePath] stringByAppendingPathComponent:@"teams.sqlite"];
        // leggiamo tutti gli elementi
        [self caricaValori];
    }
    return self;
}


// Carica i valori dal database passato come parametro
- (void)caricaValori {
    // lista temporanea
    NSMutableArray *listaTemp = [[NSMutableArray alloc] init];
    
    if (sqlite3_open([pathDB UTF8String], &database) == SQLITE_OK)
    { 
        // query che ricava i valori
        const char *sql = "select id, name, nation from teams";
        
        sqlite3_stmt *select_statement;
        
        if(sqlite3_prepare_v2(database, sql, -1, &select_statement, NULL) == SQLITE_OK) {
            
            while(sqlite3_step(select_statement) == SQLITE_ROW) {
                
                // ricaviamo i valori letti dalla query
                NSString *idTeam = [NSString stringWithUTF8String:(char *)sqlite3_column_text(select_statement, 0)];
                
                NSString *nameTeam = [NSString stringWithUTF8String:(char *)sqlite3_column_text(select_statement,1)];
                
                NSString *nationTeam = [NSString stringWithUTF8String:(char *)sqlite3_column_text(select_statement,2)];                
                
                // inseriamo tutti i valori letti in un unico oggetto
                NSDictionary *dictionary = [[NSMutableDictionary alloc] initWithObjectsAndKeys:idTeam, @"id", nameTeam, @"name", nationTeam, @"nation", nil];
                
                [listaTemp addObject:dictionary];
            }
        }
        
        self.lista = listaTemp;
        sqlite3_finalize(select_statement);
    }
    sqlite3_close(database);
}

- (void)cancellaSquadra:(NSMutableDictionary*)team {
    
    if (sqlite3_open([pathDB UTF8String], &database) == SQLITE_OK)
    {        
        // query per la cancellazione di una squadra
        NSString *query = [NSString stringWithFormat:@"DELETE FROM teams WHERE id=%@",[team objectForKey:@"id"]];
        const char *sql = [query UTF8String];
        
        sqlite3_stmt *delete_statement;
        // eseguiamo la query
        if(sqlite3_prepare_v2(database, sql, -1, &delete_statement, NULL) == SQLITE_OK) {
            
            if(sqlite3_step(delete_statement) == SQLITE_DONE) { 
                // tutto ok, ricarichiamo la lista
                [self caricaValori];
            }
        }
        // chiudiamo il db
        sqlite3_finalize(delete_statement);
    }
    sqlite3_close(database);
    
}

- (void)aggiungiSquadra:(NSMutableDictionary*)team {
    if (sqlite3_open([pathDB UTF8String], &database) == SQLITE_OK)
    {
        // query per lâ&#x20AC;&#x2122;inserimento del nuovo giocatore
        NSString *query = [NSString stringWithFormat:@"INSERT INTO teams (name, nation) VALUES ('%@','%@')", [team objectForKey:@"name"], [team objectForKey:@"nation"]];
        const char *sql = [query UTF8String];
        sqlite3_stmt *insert_statement;
        // eseguiamo la query
        if(sqlite3_prepare_v2(database, sql, -1, &insert_statement, NULL) == SQLITE_OK) {
            if(sqlite3_step(insert_statement) == SQLITE_DONE) {
                // ricarichiamo la lista
                [self caricaValori];
            }
        }
        // chiudiamo il db
        sqlite3_finalize(insert_statement);
    }
    sqlite3_close(database);
}


@end
