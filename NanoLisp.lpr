(* Scan Page 40 Col. 1 *)
PROGRAM nanolisp(INPUT, OUTPUT);

CONST
  MAXCHAINE=255;(* LES NOMS AURONT 12 CARACTERES MAXIMUM *)
  QT='''';
  GUIL='"';
  BAR='|';
  FD='->';
  GT='>';
  DOT='.';
  SPC=' ';
  PG='(';
  PD=')';
  VIDE='';
  AG='{';
  AD='}';
  PLUS='+';
  MOINS='-';
  MULT='*';
  DIVIS='/';
  DEUXP=':';
  NUL='NIL';
  TAB=CHR(9);   (* Séparateurs non-imprimables *)
  CR=CHR(13);
  LF=CHR(10);
  PROMPT1=TAB;
  PROMPT2=VIDE;
  PROMPT3='?';


TYPE
  SMALLSTRING=STRING (*[MAXCHAINE]*) ;
  TYPTOKEN=(PGAUCHE, PDROITE,APOS, SYMBOLE);
  (* 5 TYPES DE CARACTERES LUS : PARENTHESE GAUCHE, DROITE, APOSTROPHE
     ET TOUS LES AUTRES *)

  TYPEBASE=(ATOME, LISTE);

  SGRAPHE = ^NOEUD;

  NOEUD=RECORD
    CASE SORTE :TYPEBASE OF
      ATOME:
        (PNAME:^SMALLSTRING; VAL: SGRAPHE);
      LISTE:
        (CAR,CDR: SGRAPHE);
    END;
    (* AVEC TOUS CES POINTEURS LE NOEUD DE BASE NE FAIT QUE 6 OCTETS !!! *)
  PTOBLIST=^TYPOBLIST;
    (* LA LISTE DES ATOMES N'EST PAS ICI UNE LISTE LISP *)

  TYPOBLIST=RECORD
    ATOME: SGRAPHE;
    LIEN: PTOBLIST
  END;

  (* Pour la compatibilité *)
  INTERACTIVE=TEXT;

VAR (* ---- Globales ---- *)
  NILE, TRU, AQUOTE, LAMBDA, S, M, PCONSOLE, ZERO, UN, NEANT:SGRAPHE;
  OBLIST: PTOBLIST;
  FINSESS, ERREUR, TRACE: BOOLEAN;

PROCEDURE PRINT(S:SGRAPHE); FORWARD;
function isNumeric(const potentialNumeric: string): boolean; forward;
(********** PREDICATS ******************)
function nullp(s:sgraphe): boolean;
begin
  nullp:=(s=NIL) or (s=NILE);
end;
function atomp(s:sgraphe): boolean;
begin
  atomp:=((not nullp(s)) and (s^.sorte=ATOME));
end;
function listp(s:sgraphe): boolean;
begin
  listp:=((not nullp(s)) and (s^.sorte=LISTE));
end;
function numberp(s:sgraphe): boolean;
begin
  numberp:=((not nullp(s)) and
            atomp(s) and
           (nil <> s^.pname) and
           (0 <>length(s^.pname^)) and
           isNumeric(s^.pname^));
end;
function quotep(s:sgraphe): boolean;
begin
  quotep:=((not nullp(s)) and (s = aquote));
end;
function autoevaluatedp(s:sgraphe): boolean;
begin
  autoEvaluatedp :=(not nullp(s) and (s=(s^.val)));
end;
function variablep(s:sgraphe): boolean;
begin
  variablep:=(not nullp(s)) and
       atomp(s) and
       (not numberp(s)) and
       autoevaluatedp(s^.val);
end;

(***** FONCTIONS UTILITAIRES ******)
function isNumeric(const potentialNumeric: string): boolean;
(* Empruntée à RosettaCode.org *)
var
   potentialInteger: integer;
   potentialReal: real;
   integerError: integer;
   realError: integer;
begin
  integerError := 0;
  realError := 0;

   (* system.val attempts to convert numerical value representations.
      It accepts all notations as they are accepted by the language,
      as well as the '0x' (or '0X') prefix for hexadecimal values. *)
   val(potentialNumeric, potentialInteger, integerError);
   val(potentialNumeric, potentialReal, realError);

   isNumeric :=((integerError = 0) or (realError = 0));
end;
function nameOf(S:sgraphe): SMALLSTRING;
begin
   nameOf:=S^.PNAME^;
end;
function valueOf(S:sgraphe): SGRAPHE;
begin
   (* iUtilisable seulement à droite du := *)
   valueOf:=S^.VAL;
end;

(***** FONCTIONS UTILITAIRES ******)

FUNCTION FERREUR(MESSAGE:STRING; S:SGRAPHE) :SGRAPHE;
(* IMPRIME LE MESSAGE D'ERREUR ET LA LISTE OU L'ATOME EN CAUSE *)
BEGIN
  WRITE('*** ERREUR : ', MESSAGE, SPC);
  PRINT(S);
  WRITE('***');
  WRITELN;
  ERREUR:= TRUE;
  FERREUR:= NILE;
END;

PROCEDURE OBPRINT;
(* IMPRIME LA LISTE DES ATOMES CONNUS *)
VAR
  OBCOUR: PTOBLIST;

BEGIN
  OBCOUR:=OBLIST;
  WHILE (OBCOUR<>NIL) DO
    BEGIN
      IF atomp(OBCOUR^.ATOME) THEN
        WRITE(OBCOUR^.ATOME^.PNAME^,SPC);
        OBCOUR:=OBCOUR^.LIEN;
    END;
    WRITELN;
END;

FUNCTION NOUVATOM(POSITION:PTOBLIST;NOM:SMALLSTRING):PTOBLIST;
(* INSERE UN NOUVEL ATOME DANS LA LISTE A LA SUITE DE "POSITION" *)
VAR
  OBPREC,OBSUIV: PTOBLIST;
BEGIN
  (* ON REPERE LES 2 VOISINS *)
  OBPREC:=POSITION;
  (* Scan Page 40 Col. 2 *)
  OBSUIV:=OBPREC^.LIEN;
  (* ON CREE LE NOUVEL ATOME *)
  NEW(POSITION);
  NEW(POSITION^.ATOME);
  POSITION^.ATOME^.SORTE:= ATOME;
  NEW(POSITION^.ATOME^.PNAME);
  POSITION^.ATOME^.PNAME^:=NOM;
  (* est-il auto-évalué ? *)
  if numberp(POSITION^.ATOME) then   (* oui si c'est un nombre... *)
     POSITION^.ATOME^.VAL:=POSITION^.ATOME
  else
     POSITION^.ATOME^.VAL:= NIL;
  (* ON RACCROCHE DANS LA CHAINE *)
  OBPREC^.LIEN:=POSITION;
  POSITION^.LIEN:=OBSUIV;
  (* ON REND L'ATOME CREE *)
  NOUVATOM:=POSITION;
END;

FUNCTION FINDATOM(NOM: SMALLSTRING) : SGRAPHE;
(* TROUVE L'ATOME DU NON DONNE OU REND NIL *)
VAR
  CONT:BOOLEAN;
  PTCOUR: PTOBLIST;

BEGIN
  PTCOUR :=OBLIST;
  CONT:=TRUE;
  WHILE (PTCOUR<>NIL) AND CONT DO
  BEGIN
    CONT:=(PTCOUR^.ATOME^.PNAME^<>NOM);
    IF CONT THEN PTCOUR:=PTCOUR^.LIEN;
  END;
  IF PTCOUR=NIL THEN
    FINDATOM:=NILE
  ELSE
    FINDATOM:=PTCOUR^.ATOME;
END;

(**********FONCTIONS DE BASE ***********)
FUNCTION FCAR(S:SGRAPHE) : SGRAPHE;
(* LE CAR *)
BEGIN
  IF nullp(S) THEN
    FCAR:=NILE
  ELSE
    IF listp(S) THEN
      FCAR:=S^.CAR
    ELSE
      if numberp(S) then
        fcar:=S
      else
        FCAR:=FERREUR('CAR',S);
END;

FUNCTION FCDR(S:SGRAPHE):SGRAPHE; (* LE CDR *)

BEGIN
  IF nullp(S) THEN
    FCDR:=NILE
  ELSE
    IF listp(S) THEN
      FCDR:=S^.CDR
    ELSE
      if numberp(S) then
        fcdr:=NILE
      else
        FCDR:= FERREUR('CDR',S);
END;

FUNCTION FATOM(S:SGRAPHE) : SGRAPHE;
(* REND TRU SI LE SGRAPHE EST UN ATOME *)

BEGIN
  IF atomp(S) THEN
    FATOM:=TRU
  ELSE
    FATOM:=NILE;
END;

FUNCTION FEQ(S1, S2: SGRAPHE): SGRAPHE;
(* TESTE LEGALITE DES POINTEURS *)

BEGIN
  IF S1=S2 THEN
    FEQ:=TRU
  ELSE
    FEQ:=NILE;
END;

FUNCTION FCONS(S1,S2: SGRAPHE): SGRAPHE;
(* LE CONS *)
(* S2 doit être une liste ou NILE ou un nombre *)
VAR
  NEWS: SGRAPHE;
  (* Scan Page 41 Col. 1 *)

BEGIN
  if not (nullp(S2) or listp(s2) or numberp(s2)) then
    FCONS:=FERREUR('CONS', S2)
  else (* NILE ou liste ou nombre *)
    if numberp(s2) then
      FCONS:=FCONS(S1,FCONS(S2,NILE))
    else
      BEGIN
        NEW(NEWS);
        NEWS^.SORTE:=LISTE;
        NEWS^.CAR:=S1;
        NEWS^.CDR:=S2;
        FCONS:=NEWS;
      END;
END;

FUNCTION FDE (S: SGRAPHE): SGRAPHE;
  (* DONNE UN NOM A UNE FONCTION *)
  BEGIN
    S^.CAR^.VAL:=FCONS(LAMBDA,FCDR(S));
    FDE:=FCAR(S);
  END;
function fopari(const op:string;s:sgraphe): sgraphe;
var
  iop1, iop2, testReal: real;
  testInt:integer;
  resultat: string;
  errCode1, errCode2: integer;
  s1, s2: sgraphe;
begin
  if nullp(s) then (* on retourne l'element neutre de l'operation *)
    begin
      if (op=PLUS) or (op=MOINS) then
        fopari:=zero
      else
        fopari:=un
    end
  else
  begin
    if atomp(s) then
      begin
        if numberp(s) then
          val(nameOf(s), iop1, errCode1);
          if errCode1=0 then
            begin
              (* Si le nombre existe, on le réutilise *)
              s1:=FINDATOM(nameOf(s));
              if not nullp(s1) then
                fopari:=s1
              else
                (* Sinon on le crée *)
                fopari:=nouvatom(oblist, nameOf(s))^.atome;
            end
        else
        begin
          fopari:=ferreur(op, s);
        end;
      end
    else (* Reduction de la liste par l'operation *)
      begin
        s1:=fopari(op,fcar(s));
        val(nameOf(s1),iop1,errCode1);
        s2:=fopari(op,fcdr(s));
        val(nameOf(s2),iop2,errCode2);
        if (errCode1<>0) or (errCode2<>0) then
          fopari:=ferreur(op,s)
        else
        begin
           case op of
             PLUS:str(iop1 + iop2, resultat);
             MOINS:str(iop1 - iop2, resultat);
             MULT:str(iop1 * iop2, resultat);
             DIVIS: str(iop1 / iop2, resultat);
           end;
           val(resultat, testReal, errCode1);
           testInt:=round(testReal);
           if ((abs(testReal - testInt)<1.0E-10) and (errCode1=0)) then
             (* Convertir en entier *)
             begin
               str(testInt, resultat);
             end;
           s1:=FINDATOM(resultat);
           if not nullp(s1) then
             fopari:=s1
           else
             fopari:=NOUVATOM(oblist, resultat)^.atome;
        end;
      end;
  end;
end;
function fadd(s:sgraphe): sgraphe;
begin
  fadd:=fopari(PLUS,s);
end;
function fsub(s:sgraphe): sgraphe;
begin
  fsub:=fopari(MOINS,s);
end;
function fmult(s:sgraphe): sgraphe;
begin
  fmult:=fopari(MULT,s);
end;
function fdiv(s:sgraphe): sgraphe;
begin
  fdiv:=fopari(DIVIS,s);
end;
FUNCTION EVLIS(ARGS: SGRAPHE): SGRAPHE; FORWARD;
FUNCTION FSETQ(S:SGRAPHE): SGRAPHE;
BEGIN
  IF atomp(S^.CAR) AND not nullp(S^.CAR) THEN
    BEGIN
      S^.CAR^.VAL:=FCAR(EVLIS(FCDR(S)));
      FSETQ:=S^.CAR^.VAL;
    END
  ELSE
    BEGIN
      FSETQ:=FERREUR('SETQ',S);
    END;
END;

FUNCTION FNULL(S:SGRAPHE): SGRAPHE;
BEGIN
  IF S=NILE THEN
    FNULL:=TRU
  ELSE
    FNULL:=NILE
END;
(*********** PROCEDURES D'ENTREE-SORTIES *****)
FUNCTION FREAD(VAR INFILE: INTERACTIVE): SGRAPHE;
(* LE NOM READ EST DEJA RESERVE PAR PASCAL *)
VAR
  (*LUGRAPHE:SGRAPHE;*)
  TAMPON:CHAR; (* DERNIER CARACTERE LU *)
  TOKEN: TYPTOKEN; (* TYPE DU CARACTERE LU *)

  FUNCTION READATOM(VAR TOKEN: TYPTOKEN): SGRAPHE;

  VAR
    LUCHAINE: SMALLSTRING;
    LUATOME: SGRAPHE;
    INTER: STRING[1]; (*POUR PERMETTRE L'APPLICATION DE LA FONCTION CONCAT *)
  BEGIN
    LUCHAINE:=VIDE;
    INTER:=SPC;

  (* INITIALISATIONS *)
  IF TAMPON=SPC THEN
  BEGIN
    (* TOUT CE QUI A ETE LU PRECEDMMENT A ETE TRAITE*)
    (* 1 = ON AVALE LES BLANCS *)
    WHILE (NOT EOF(INFILE)) AND (TAMPON in [SPC, TAB, LF]) DO READ(INFILE, TAMPON);
    (* 2 = ON LIT JUSQU'AU PREMIER SEPARATEUR *)
    WHILE NOT (EOF(INFILE)
      OR (TAMPON IN [PG, PD, QT, SPC, TAB, LF])
      OR (LENGTH(LUCHAINE) >= MAXCHAINE)) DO
      BEGIN INTER[1]:= TAMPON;
        LUCHAINE:=CONCAT(LUCHAINE, INTER);
        READ (INFILE,TAMPON);
      END(*WHILE*)
    (* ON A MAINTENANT LE NOM LU OU RIEN DANS LUCHAINE ET LE SEPARATEUR DANS TAMPON *)
  END; (* IF TAMPON *)

  IF (LUCHAINE=VIDE) THEN
     BEGIN
    (* CAS OU UN SEPARATEUR EST LU EN PREMIER *)
    (* TRANSFER DANS LUCHAINE *)
      INTER[1]:=TAMPON;
      LUCHAINE:=CONCAT(LUCHAINE,INTER);
      TAMPON:=SPC;
    END;

    (* INITIALISATIONS *)
    READATOM:=NILE;
    IF LUCHAINE=PG     THEN TOKEN:=PGAUCHE ELSE
    IF LUCHAINE=PD     THEN TOKEN:=PDROITE ELSE
    IF LUCHAINE=QT     THEN TOKEN:=APOS ELSE
      BEGIN
        (* Scan Page 41 Col. 2 *)
        (*UN NOM A ETE LU - EST-IL CONNU ? *)
        TOKEN:=SYMBOLE;
        LUATOME:=FINDATOM(LUCHAINE);
        (* C'EST UN NOUVEAU NOM -> ON L'ENREGISTRE *)
        IF nullp(LUATOME) THEN
          READATOM:=NOUVATOM(OBLIST, LUCHAINE)^.ATOME
        ELSE
          READATOM:=LUATOME;
      END; (*ELSE LUCHAINE*)
  END;

  FUNCTION READ1(DANSLISTE: BOOLEAN): SGRAPHE;
  (* READ1 LIT 2 TYPES DE "PHRASES":
    ATOMES : MOTS EN DEHORS DE PARANTHESE
    LISTES : SUITE DE MOTS ENTRE PARENTHESES *)
  VAR
    LUGRAPHE, R1, R2: SGRAPHE;

  BEGIN
    LUGRAPHE:=READATOM(TOKEN);
    CASE TOKEN OF
      PGAUCHE: IF DANSLISTE THEN
        BEGIN
          (* LECTURE DU CAR ET DU CDR *)
          R1:=READ1(TRUE);
          R2:=READ1(TRUE);
          (* ASSEMBLAGE *)
          READ1:=FCONS(R1,R2);
        END ELSE
        (* ON DEMARRE LA LECTURE D'UNE NOUVELLE LISTE *)
          READ1:=READ1(TRUE);
      PDROITE: READ1:=NILE; (* ON A TERMINE LA LISTE *)
      APOS: IF DANSLISTE THEN
        BEGIN (* ON LIT UN TERME DE TYPE ATOME ET UN DE TYPE LISTE *)
          R1:=READ1(FALSE);
          R2:=READ1(TRUE);
          (* ON LES ASSEMBLE *)
          READ1:=FCONS(FCONS(AQUOTE,FCONS(R1,NILE)), R2);
        END ELSE
          BEGIN
            (*ON LIT UN TERME DE TYPE ATOME SEULEMENT *)
            R1:=READ1(FALSE);
        (* ON LE REND SOUS LA FORME (QUOTE) (ATOME) *)
            READ1:=FCONS(AQUOTE,FCONS(R1,NILE));
          END;
      SYMBOLE: IF DANSLISTE THEN
        BEGIN
          (* ON LIT L'ATOME ET ON CONTINUE JUSQU'A FIN DE LA LISTE *)
          R1:=READ1(TRUE);
          READ1:=FCONS(LUGRAPHE,R1);
        END ELSE (* LECTURE D'UN ATOME SEUL *)
          READ1 :=LUGRAPHE;
    END; (* CASE *)
  END;
(* Corps de FREAD *)
BEGIN
  TAMPON:=SPC;
  FREAD:=READ1(FALSE);
END; (*FREAD*)

PROCEDURE PRINT(S:SGRAPHE);
  (* POUR IMPRIMER DES LISTES *)
  (* Scan Page 42 Col. 1 *)
  VAR
     BLANCPRINT: BOOLEAN;
     PROCEDURE PRINT1(S:SGRAPHE);
       PROCEDURE PRINTATOM(S:SGRAPHE);
         BEGIN
           IF BLANCPRINT THEN WRITE(SPC);
           (* LE NOM DE L'ATOME *)
           if not nullp(S) then
              WRITE(nameOf(S))
           else
              WRITE(NUL,SPC);
           BLANCPRINT:=FALSE;
         END;
       (* Corps de PRINT1 *)
       BEGIN
          IF nullp(S) THEN
             PRINTATOM(NILE)
          ELSE
              IF atomp(S) THEN
                 BEGIN
                 (* IMPRESSION DE L'ATOME SEUL *)
                    PRINTATOM(S);
                    BLANCPRINT:=TRUE;
                 END ELSE
                 BEGIN
                    IF quotep(FCAR(S)) THEN
                    BEGIN
                      (* TRANSFORMATION INVERSE DE LA LECTURE :
                      ((QUOTE) (ATOME)) => '(ATOME)
                      *)
                      WRITE(SPC,QT);
                      PRINT(FCAR(FCDR(S))); (* ON REPART COMME POUR UNE NOUVELLE LISTE *)
                    END ELSE
                    BEGIN
                      IF (NOT nullp(S^.CAR)) AND listp(S^.CAR) AND not quotep(S^.CAR^.CAR) THEN
                         WRITE(PG); (* SUITE DE LISTE *)

                      PRINT1(FCAR(S)); (* IMPRESSION DU CAR *)

                      IF nullp(FCDR(S)) THEN
                         WRITE(PD) (* FIN DE LISTE *)
                      ELSE
                         PRINT1(FCDR(S)); (* IMPRESSION DU CDR *)
                    END; (* NOT QUOTE *)
                 END;
       END;
  (* Corps de PRINT *)
   BEGIN
     BLANCPRINT:=FALSE;
     IF listp(S) AND not quotep(S^.CAR) THEN
        WRITE(PG);
     PRINT1(S);
   END;

PROCEDURE PAIRLIS(VAR NOMS, VALS: SGRAPHE);
(* LES VALEURS PASSEES SONT MOMENTANEMENT ASSOCIEES AUX NOMS
  CEUX-CI RETROUVERONT LEURS ANCIENNES VALEURS PAR
  L'OPERATION INVERSE.
  PAIRLIS N'EST APPELEE QUE LORS DE L'EXECUTION DE LAMBDA
*)
  procedure swap(VAR S1,S2:sgraphe);
  var
    save: sgraphe;
  begin
    save:=S1;
    S1:=S2;
    S2:=save;
  end;

BEGIN
  IF ERREUR THEN EXIT(*PAIRLIS*);
  IF TRACE THEN BEGIN
    WRITELN;
    WRITE(TAB,'PAIRLIS ', GT); PRINT(NOMS);
    WRITE(DOT);
    PRINT(VALS); WRITELN;
  END;
  IF listp(NOMS) THEN
    BEGIN
      PAIRLIS(NOMS^.CDR,VALS^.CDR);
      swap(NOMS^.CAR^.VAL,VALS^.CAR);
    END
  ELSE
    IF NOT nullp(NOMS) THEN
       swap(NOMS^.VAL, vals);
       (* Scan Page 42 Col. 2 *)
END;

FUNCTION EVAL(E:SGRAPHE):SGRAPHE;FORWARD;
FUNCTION FLOAD (FILENAME:SGRAPHE):SGRAPHE;
VAR
  INFILE: INTERACTIVE;
  fichier: string;
  s1, s2: sgraphe;

BEGIN

  fichier:=nameOf(FILENAME);
  IF TRACE THEN
    BEGIN
      WRITELN;
      WRITE(TAB,'LOADING',FD);
      WRITE(fichier);
      WRITELN;
  END;
  IF  atomp(FILENAME) AND not nullp(FILENAME) THEN
    BEGIN
      if FILENAME <> PCONSOLE then
        begin
          (* Ajouté pour compatibilité Free Pascal *)
          ASSIGN(INFILE,fichier);
          RESET (INFILE);
        end
      else
        INFILE:=INPUT;
      WRITE(PROMPT1);
      WHILE NOT EOF (INFILE) and not (ERREUR or FINSESS) DO
        BEGIN
          s1:=FREAD(INFILE);
          WRITE(PROMPT2);
          s2:=EVAL(s1);
          PRINT(s2);
          WRITELN;
          WRITE(PROMPT1);
        END;
        CLOSE(INFILE);
        FLOAD:=TRU;
    END ELSE
      FLOAD:=FERREUR('LOAD',FILENAME);
END;

FUNCTION APPLISTE(S:SGRAPHE): SGRAPHE;
(* EXECUTE UNE SUITE D'EXPRESSIONS ET REND LA DERNIERE *)

BEGIN
  REPEAT
    IF ERREUR THEN
      BEGIN
        APPLISTE:=NILE;
        EXIT(*APPLISTE*);
      END;
    APPLISTE:=EVAL(FCAR(S));
    S:=FCDR(S);
  UNTIL ERREUR OR (S=NILE)
END;

FUNCTION APPLY(FN,ARGS:SGRAPHE):SGRAPHE;
(* EXECUTE UNE FONCTION AVEC LES ARGUMENTS PASSES *)

BEGIN
  IF ERREUR THEN
    BEGIN
      APPLY:=FN;
      EXIT(*APPLY*);
    END;
  IF TRACE THEN
    BEGIN
      WRITELN;
      WRITE(TAB,'APPLY',GT);
      PRINT(FN);
      WRITE(FD);
      PRINT(ARGS);
      WRITELN;
    END;
    IF nullp(FN) THEN
      (* APPLY:=FERREUR('APPLY', FN) *)
      begin
        apply:=NILE;
        exit;
      end
    ELSE IF atomp(FN) THEN
      (* FONCTIONS PREDEFINIES *)
      IF nameOf(FN)='CAR'       THEN APPLY:=FCAR(FCAR(ARGS)) ELSE
      IF nameOf(FN)='CDR'       THEN APPLY:=FCDR(FCAR(ARGS)) ELSE
      IF nameOf(FN)='CONS'      THEN APPLY:=FCONS(FCAR(ARGS), FCAR(FCDR(ARGS))) ELSE
      IF nameOf(FN)='ATOM'      THEN APPLY:=FATOM(FCAR(ARGS)) ELSE
      IF nameOf(FN)='EQ'        THEN APPLY:=FEQ(FCAR(ARGS),FCAR(FCDR(ARGS))) ELSE
      IF nameOf(FN)=PLUS         THEN APPLY:=FADD(EVAL(ARGS)) ELSE
      IF nameOf(FN)=MOINS        THEN APPLY:=FSUB(EVAL(ARGS)) ELSE
      IF nameOf(FN)=MULT         THEN APPLY:=FMULT(EVAL(ARGS)) ELSE
      IF nameOf(FN)=DIVIS        THEN APPLY:=FDIV(EVAL(ARGS)) ELSE
      IF nameOf(FN)='READ'      THEN APPLY:=FREAD(INPUT) ELSE
      IF nameOf(FN)='PRINT'     THEN BEGIN
                                        PRINT(FCAR(ARGS));
                                        APPLY:=NEANT;
                                     END ELSE
      IF nameOf(FN)='OBLIST'    THEN BEGIN
                                        APPLY:=NILE;
                                        OBPRINT;
                                     END ELSE
      IF nameOf(FN)='QUIT'      THEN BEGIN
                                        APPLY:=NILE;
                                        FINSESS:=TRUE;
                                     END ELSE
      IF nameOf(FN)='LOAD'      THEN BEGIN
                                       if (quotep(fcar(fcar(args)))) then
                                         apply:=fload(fcar(fcdr(fcar(args))))
                                       else
                                         APPLY:=FLOAD(FCAR(ARGS))
                                       END ELSE
            (* CE N'EST PAS UNE FONCTION PREDEFINIE
            ON OBTIENT SA DEFINITION PAR EVAL ET ON APPLIOUE *)
      APPLY:=APPLY(EVAL(FN),ARGS)
  ELSE
    (* FN est une liste *)
    IF FCAR(FN)=LAMBDA THEN
      BEGIN
        (* Scan Page 43 Col. 1 *)
        PAIRLIS(FN^.CDR^.CAR,ARGS); (* ON DONNE LEUR VALEUR AUX PARAMETRES *)
        APPLY:=APPLISTE(FCDR(FCDR(FN)));
        PAIRLIS(FN^.CDR^.CAR,ARGS);(* ON RESTAURE LEURS ANCIENNES VALEURS *)
      END ELSE
    IF FCAR(FN)=AQUOTE THEN
    BEGIN
      APPLY:=FCONS(FN, ARGS);
    END ELSE
        APPLY:=FERREUR('APPLY', FN);
END; (*APPLY*)

FUNCTION EVLIS(ARGS: SGRAPHE): SGRAPHE;
(* EVALUE LES COMPOSANTS D'UNE LISTE *)

BEGIN
  IF nullp(ARGS) THEN
    EVLIS:=NILE
  ELSE
    EVLIS:=FCONS(EVAL(FCAR(ARGS)), EVLIS(FCDR(ARGS)));
END;

FUNCTION EVCOND(L:SGRAPHE):SGRAPHE;
(*EXECUTE UNE CONDITIONNELLE *)

BEGIN
  IF NOT nullp(EVAL(FCAR(FCAR(L)))) THEN
    BEGIN
      IF TRACE THEN
         WRITE(TAB,'COND IF THEN', SPC);
      EVCOND:=APPLISTE(FCDR(FCAR(L)));
      END
  ELSE
    IF NOT nullp(FCDR(L)) THEN
      BEGIN
        IF TRACE THEN
           WRITE(TAB,'COND IF ELSE', SPC);
        EVCOND:=EVCOND(FCDR(L));
      END
    ELSE
      EVCOND:=NILE;
    END;


(*********** LA FONCTION EVAL **********)
FUNCTION EVAL(E:SGRAPHE): SGRAPHE;
(* L'EVALUATEUR :
  SI LE PARAMETRE EST UN ATOME -> REND SA VALEUR SINON
  SI LE CAR EST UN ATOME
  SI C'EST UNE "FONCTION SPECIALE"
    (PARAMETRES NON EVALUES) -> EXECUTION
  SINON -> APPELLE APPLY POUR SON EXECUTION AVEC LA LISTE
    EVALUEE DES ARGUMENTS *)
VAR
  S:SGRAPHE;
BEGIN
  IF ERREUR THEN
    BEGIN
      EVAL:=E;
      EXIT(*EVAL*);
    END;
  IF TRACE THEN
    BEGIN
      WRITELN;
      WRITE(TAB,'EVAL',GT);
      PRINT(E);
      WRITELN;
    END;
  IF atomp(E) THEN
    IF nullp(E) or nullp(valueOf(E))THEN
      EVAL:=NILE
    ELSE
      EVAL:=valueOf(E)
  ELSE BEGIN
    S:=FCAR(E);
    IF atomp(S)                THEN
      if variablep(s) or
         numberp(s)             then EVAL:=FCONS(EVAL(S),EVLIS(FCDR(E))) else
      IF nameOf(S)='QUOTE'      THEN EVAL:=E ELSE
      IF nameOf(S)='COND'       THEN EVAL:=EVCOND(FCDR(E)) ELSE
      IF nameOf(S)='TRACE'      THEN BEGIN
                                      TRACE:=TRUE;
                                      EVAL:=FINDATOM('TRACE') END ELSE
      IF nameOf(S)='UNTRACE'    THEN BEGIN
                                      TRACE:=FALSE;
                                      (* Scan Page 43 Col. 2 *)
                                      EVAL:=FINDATOM('UNTRACE') END ELSE
      IF nameOf(S)='SETQ'       THEN EVAL:=FSETQ(FCDR(E)) ELSE
      IF nameOf(S)='DE'         THEN EVAL:=FDE(FCDR(E)) ELSE
        begin
             if trace then
             begin
               WRITE(TAB,TAB,'EVAL', GT); PRINT(S); WRITE(FD);PRINT(FCDR(E)); WRITELN;
             end;
             EVAL:=APPLY(S,EVLIS(FCDR(E)))
        end;
  END;
  if trace then
    begin
      write(tab,'=');print(eval);writeln;
    end;
END;

(******* INITIALISATION ******)
PROCEDURE INIT;

VAR
  OBCOUR, PPCONSOLE:PTOBLIST;

BEGIN
  TRACE:=FALSE;
  (* CREATION DU PREMIER ATOME *)
  NEW(NILE);
  NEW(NILE^.PNAME);
  NILE^.PNAME^:='()';
  NEW(OBCOUR);
  OBCOUR^.ATOME:=NILE;
  (* ON CONSERVE L'ENTREE DANS LA LISTE AVEC OBLIST *)
  OBLIST:=OBCOUR;
  OBCOUR:=NOUVATOM(OBCOUR, 'T'); TRU:=OBCOUR^.ATOME;
  OBCOUR:=NOUVATOM(OBCOUR, 'QUOTE'); AQUOTE:=OBCOUR^.ATOME;
  OBCOUR:=NOUVATOM(OBCOUR, 'CAR');
  OBCOUR:=NOUVATOM(OBCOUR, 'CDR');
  OBCOUR:=NOUVATOM(OBCOUR, 'CONS');
  OBCOUR:=NOUVATOM(OBCOUR, 'ATOM');
  OBCOUR:=NOUVATOM(OBCOUR, 'EQ');
  OBCOUR:=NOUVATOM(OBCOUR, 'LAMBDA'); LAMBDA:=OBCOUR^.ATOME;
  OBCOUR:=NOUVATOM(OBCOUR, 'READ');
  OBCOUR:=NOUVATOM(OBCOUR, 'PRINT');
  OBCOUR:=NOUVATOM(OBCOUR, 'COND');
  OBCOUR:=NOUVATOM(OBCOUR, 'TRACE');
  OBCOUR:=NOUVATOM(OBCOUR, 'UNTRACE');
  OBCOUR:=NOUVATOM(OBCOUR, 'SETQ');
  OBCOUR:=NOUVATOM(OBCOUR, 'LOAD');
  OBCOUR:=NOUVATOM(OBCOUR, 'OBLIST');
  OBCOUR:=NOUVATOM(OBCOUR, 'QUIT');
  OBCOUR:=NOUVATOM(OBCOUR, '0'); ZERO:=OBCOUR^.ATOME;
  OBCOUR:=NOUVATOM(OBCOUR, '1'); UN:=OBCOUR^.ATOME;
  OBCOUR:=NOUVATOM(OBCOUR, VIDE); NEANT:=OBCOUR^.ATOME;

  TRU^.VAL:=TRU;
  NILE^.VAL:=NILE;
  NEW(PPCONSOLE);
  PPCONSOLE:=NOUVATOM(PPCONSOLE, 'CONSOLE');
  PCONSOLE:=PPCONSOLE^.ATOME;
END;(*INIT*)

(* Corps du programme principal *)
BEGIN
    FINSESS:=FALSE;
    INIT;
    WRITELN('NANO-LISP - (C)JM HUSSON (1985), JPDS (2021)');

    REPEAT
      M:=FLOAD(PCONSOLE);
      ERREUR :=FALSE;
      S:=EVAL(M);
      IF NOT (FINSESS OR ERREUR) THEN
      BEGIN
        PRINT(S);
      END;
      WRITELN;
    UNTIL FINSESS OR ERREUR;
END.
