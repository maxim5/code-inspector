{dep/funcoes/funcoesgerais.i}

DEF VAR reg AS CHAR EXTENT 10.
DEF VAR vok AS LOG FORMAT "S/N".
DEF VAR cont AS INT INITIAL 0.

DEF STREAM ssaida.

DEF TEMP-TABLE tt-temp
    FIELD tabela        AS CHAR
    FIELD codigo        AS CHAR
    FIELD descricao     AS CHAR
    FIELD vl_hm         AS CHAR
    FIELD vl_co         AS CHAR
    FIELD qt_auxiliar   AS CHAR
    FIELD pan           AS CHAR
    FIELD filme         AS CHAR
    FIELD aba           AS CHAR
    FIELD obs           AS CHAR
    FIELD existe        AS LOG FORMAT "S/N".

DEF TEMP-TABLE tt-cobertura
    FIELD cd-modalidade AS CHAR
    FIELD cd-plano      AS CHAR
    FIELD cd-tipo-plano AS CHAR
    FIELD cd-modulo     AS CHAR
    FIELD cd-amb        AS CHAR.

PAUSE(2).
INPUT STREAM ssaida FROM 'c:\spool\verifica_cadastro_cobertura.csv'.
REPEAT:
    cont = cont + 1.
    MESSAGE "[PROCESSANDO REGISTRO] : " + STRING(cont).

    reg = "".
    IMPORT STREAM ssaida DELIMITER ';' reg.
    IF NOT fnEhNumero(reg[2]) THEN NEXT.

    vok = NO.
   
    IF CAN-FIND( LAST  pl-mo-am
                 WHERE pl-mo-am.cd-amb = INT(reg[2])
                 AND   (   pl-mo-am.cd-modalidade = 10
                        OR pl-mo-am.cd-modalidade = 30
                        OR pl-mo-am.cd-modalidade = 40
                        OR pl-mo-am.cd-modalidade = 50
                        OR pl-mo-am.cd-modalidade = 60)) THEN vok = YES.

    CREATE tt-temp.
    ASSIGN
        tt-temp.tabela      = reg[1]
        tt-temp.codigo      = reg[2]
        tt-temp.descricao   = reg[3]
        tt-temp.vl_hm       = reg[4]
        tt-temp.vl_co       = reg[5]
        tt-temp.qt_auxiliar = reg[6]
        tt-temp.pan         = reg[7]
        tt-temp.filme       = reg[8]
        tt-temp.aba         = reg[9]
        tt-temp.obs         = reg[10]
        tt-temp.existe      = vok.

    IF vok = YES THEN DO:
        FOR EACH pl-mo-am
            WHERE pl-mo-am.cd-amb = INT(reg[2])
            AND  (pl-mo-am.cd-modalidade = 10
               OR pl-mo-am.cd-modalidade = 30
               OR pl-mo-am.cd-modalidade = 40
               OR pl-mo-am.cd-modalidade = 50
               OR pl-mo-am.cd-modalidade = 60) NO-LOCK:

            IF AVAIL pl-mo-am THEN DO:
                CREATE tt-cobertura.
                ASSIGN
                    tt-cobertura.cd-modalidade = STRING(pl-mo-am.cd-modalidade)
                    tt-cobertura.cd-plano      = STRING(pl-mo-am.cd-plano)
                    tt-cobertura.cd-tipo-plano = STRING(pl-mo-am.cd-tipo-plano)
                    tt-cobertura.cd-modulo     = STRING(pl-mo-am.cd-modulo)
                    tt-cobertura.cd-amb        = STRING(pl-mo-am.cd-amb).
            END.
        END.
    END.

END.
INPUT STREAM ssaida CLOSE.

cont = 0.

OUTPUT STREAM ssaida TO "c:\spool\verifica_cadastro_cobertura_preenchida.csv".
    PUT STREAM ssaida "tabela;codigo;descricao;vl hm;vl co;qt auxiliar;pan;filme;aba;obs;existe" SKIP.

    FOR EACH tt-temp:
        cont = cont + 1.
        MESSAGE "[EXPORTANDO REGISTRO] : " + STRING(cont).
        EXPORT STREAM ssaida DELIMITER ';'
            tt-temp.
    END.

OUTPUT STREAM ssaida CLOSE.

OUTPUT STREAM ssaida TO "c:\spool\verifica_cadastro_cobertura_chaves.csv".
    PUT STREAM ssaida SKIP SKIP "cd-modalidade;cd-plano;cd-tipo-plano;cd-modulo;cd-amb" SKIP.

    FOR EACH tt-cobertura:
        EXPORT STREAM ssaida DELIMITER ';'
            tt-cobertura.
    END.
OUTPUT STREAM ssaida CLOSE.
