(* Scan Page 40 Col. 1 *)
PROGRAM nanolisp(INPUT, OUTPUT);

CONST
  MAXCHAINE=255;(* LES NOMS AURONT 12 CARACTERES MAXIMUM *)
  QT='''';
  FD='~~>';
  GT='>';
  DOT='.';
  SPC=' ';
  PG='(';
  PD=')';
  VIDE='';
  PLUS='+';
  MOINS='-';
  MULT='*';
  DIVIS='/';
  NUL='NIL';
  TAB=CHR(9);   (* Séparateurs non-imprimables *)
  LF=CHR(10);
  PROMPT1=TAB;
  PROMPT2=VIDE;
  PI=4.0*ARCTAN(1.0);
  _SIN='SIN';
  _COS='COS';
  _TAN='TAN';
  _ATAN='ATAN';
  _RAC='RAC';
  _ABS='ABS';
  EPSILON=1.0E-6;
  OBSEP=SPC;
TYPE
  SMALLSTRING=STRING (*[MAXCHAINE]*) ;
  TYPTOKEN=(PGAUCHE, PDROITE,APOS, SYMBOLE);
  (* 5 TYPES DE CARACTERES LUS : PARENTHESE GAUCHE, DROITE, APOSTROPHE
     ET TOUS LES AUTRES *)

  TYPEBASE=(ATOME, LISTE);

  SGRAPHE = ^NOEUD;

  PTOBLIST=^TYPOBLIST;
  (* LA LISTE DES SEXPS N'EST PAS ICI UNE LISTE LISP *)
  TYPOBLIST=RECORD
    SEXP: SGRAPHE;
    LIEN: PTOBLIST;
   END;

  NOEUD=RECORD
    PLIST:PTOBLIST;
    CASE SORTE :TYPEBASE OF
      ATOME:
        (PNAME:^SMALLSTRING; VAL: SGRAPHE);
      LISTE:
        (CAR,CDR: SGRAPHE);
    END;
    (* AVEC TOUS CES POINTEURS LE NOEUD DE BASE NE FAIT QUE 6 OCTETS !!! *)



  (* Pour la compatibilité *)
  INTERACTIVE=TEXT;

(* ----------- Globales -------------- *)
VAR
  NILE, TRU, AQUOTE, LAMBDA, S, M, PCONSOLE, ZERO, UN, PPI, NEANT:SGRAPHE;
  OBLIST: PTOBLIST;
  FINSESS, ERREUR, TRACE: BOOLEAN;
  sPI:STRING;

(***** Definitions anticipees **********)
PROCEDURE PRINT(S:SGRAPHE); FORWARD;
function isNumeric(const potentialNumeric: string): boolean; FORWARD;
FUNCTION EVAL(E:SGRAPHE):SGRAPHE;FORWARD;

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
function integerp(testReal, epsilon:real): boolean;
begin
  integerp:=(abs(testReal - trunc(testReal)) < epsilon);
end;

(******* FONCTIONS UTILITAIRES ********)
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
   (* Pour eliminer le warning à la compil' *)
   if potentialInteger * potentialReal <> 0 then ;
   isNumeric :=((integerError = 0) or (realError = 0));
end;
function nameOf(S:sgraphe): SMALLSTRING;
begin
  nameOf:=S^.PNAME^
end;
function valueOf(S:sgraphe): SGRAPHE;
begin
   (* Utilisable seulement à droite du := *)
      valueOf:=S^.VAL
end;
function degresEnRadians(degres:real):real;
begin
  degresEnRadians:=(degres/180.0)*PI;
end;
function radiansEnDegres(radians:real):real;
begin
  radiansEnDegres:=(radians/PI)*180.0;
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

PROCEDURE OBPRINT1(OBCOUR: PTOBLIST);
(* IMPRIME LA LISTE DES SEXPS CONNUS d'une liste donnee *)

BEGIN
  WHILE (OBCOUR<>NIL) DO
    BEGIN
      (*WRITE(nameOf(OBCOUR^.SEXP), ': '),nameOf(valueOf(OBCOUR^.SEXP)), SPC);*)
      PRINT(OBCOUR^.SEXP);
      OBCOUR:=OBCOUR^.LIEN;
    END;
    WRITELN;
END;
PROCEDURE OBPRINT;
(* IMPRIME LA LISTE DES SEXPS CONNUSv*)
BEGIN
   OBPRINT1(OBLIST);
END;

FUNCTION NOUVATOM(POSITION:PTOBLIST;NOM:SMALLSTRING):PTOBLIST;
(* INSERE UN NOUVEL SEXP DANS LA LISTE A LA SUITE DE "POSITION" *)
VAR
  OBPREC,OBSUIV: PTOBLIST;
BEGIN
  (* ON REPERE LES 2 VOISINS *)
  OBPREC:=POSITION;
  (* Scan Page 40 Col. 2 *)
  OBSUIV:=OBPREC^.LIEN;
  (* ON CREE LE NOUVEL SEXP *)
  NEW(POSITION);
  NEW(POSITION^.SEXP);
  POSITION^.SEXP^.SORTE:= ATOME;
  NEW(POSITION^.SEXP^.PNAME);
  POSITION^.SEXP^.PNAME^:=NOM;
  NEW(POSITION^.SEXP^.PLIST);
  POSITION^.SEXP^.PLIST^.SEXP:=NILE;
  (* est-il auto-évalué ? *)
  if numberp(POSITION^.SEXP) then   (* oui si c'est un nombre... *)
     POSITION^.SEXP^.VAL:=POSITION^.SEXP
  else
     POSITION^.SEXP^.VAL:=NIL;
  (* ON RACCROCHE DANS LA CHAINE *)
  OBPREC^.LIEN:=POSITION;
  POSITION^.LIEN:=OBSUIV;
  (* ON REND L'SEXP CREE *)
  NOUVATOM:=POSITION;
END;

FUNCTION FINDSEXP2(NOM: SMALLSTRING; PTCOUR:PTOBLIST) : SGRAPHE;
(* TROUVE L'SEXP DU NOM DONNE dans une liste donnee OU REND NIL *)
VAR
  CONT:BOOLEAN;

BEGIN
  CONT:=TRUE;
  WHILE (PTCOUR<>NIL) AND CONT DO
  BEGIN
    CONT:=(nameOf(PTCOUR^.SEXP)<>NOM);
    IF CONT THEN PTCOUR:=PTCOUR^.LIEN;
  END;
  IF PTCOUR=NIL THEN
    FINDSEXP2:=NILE
  ELSE
    FINDSEXP2:=PTCOUR^.SEXP;
END;
FUNCTION FINDSEXP(NOM: SMALLSTRING) : SGRAPHE;
(* TROUVE L'ATOME DU NOM DONNE OU REND NIL *)
BEGIN
   FINDSEXP:=FINDSEXP2(NOM, OBLIST);
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
FUNCTION FNEQ(S1, S2: SGRAPHE): SGRAPHE;
(* TESTE LEGALITE DES POINTEURS *)

BEGIN
  IF S1<>S2 THEN
    FNEQ:=TRU
  ELSE
    FNEQ:=NILE;
END;
(* Comparaisons de valeurs numeriques => TRU ou NILE *)
function fcompar(const op:string;s:sgraphe): sgraphe;
var
  rop1, rop2: real;
  ok:boolean;
  errCode1, errCode2: integer;
  s1, s2, s3: sgraphe;
begin
  if nullp(s) then (* on retourne l'element neutre de l'operation *)
     fcompar:=TRU
  else
  begin
    if atomp(s) or nullp(fcar(fcdr(s))) then
       fcompar:=TRU
    else
    (* Une liste: < '(1 10 60) est vrai, car tous ordonnés *)
      begin
        s1:=fcar(s);
        s2:=fcar(fcdr(s));
        s3:=fcompar(op, fcdr(s));
        val(nameOf(s1),rop1,errCode1);
        val(nameOf(s2),rop2,errCode2);
        if (errCode1<>0) or (errCode2<>0) then
          fcompar:=ferreur(op,s)
        else
          begin
           case op of
             '>' :ok:=(rop1 > rop2) and (s3=TRU);
             '>=':ok:=(rop1 >= rop2) and (s3=TRU);
             '<' :ok:=(rop1 < rop2) and (s3=TRU);
             '<=':ok:=(rop1 <= rop2) and (s3=TRU);
           end;
           if ok then
             fcompar:=TRU
           else
             fcompar:=NILE;
        end;
      end;
  end;
end;

FUNCTION FGT(S: SGRAPHE): SGRAPHE;
(* COMPARE LES VALEURS *)

BEGIN
  fgt:=fcompar('>', S);
END;
FUNCTION FGE(S:SGRAPHE): SGRAPHE;
(* COMPARE LES VALEURS *)

BEGIN
  fge:=fcompar('>=', S);
END;
FUNCTION FLT(S:SGRAPHE): SGRAPHE;
(* COMPARE LES VALEURS *)

BEGIN
  flt:=fcompar('<', S);
END;
FUNCTION FLE(S:SGRAPHE): SGRAPHE;
(* COMPARE LES VALEURS *)

BEGIN
  fle:=fcompar('<=', S);
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
(*********** Les Quatre Operations Arithmetiques **********)
function fopari(const op:string;s:sgraphe): sgraphe;
var
  rop1, rop2, testReal: real;
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
          val(nameOf(s), rop1, errCode1);
          if errCode1=0 then
            begin
              (* Si le nombre existe, on le réutilise *)
              s1:=FINDSEXP(nameOf(s));
              if not nullp(s1) then
                fopari:=s1
              else
                (* Sinon on le crée *)
                fopari:=nouvatom(oblist, nameOf(s))^.SEXP;
            end
        else
        begin
          fopari:=ferreur(op, s);
        end;
      end
    else (* Reduction de la liste par l'operation *)
      begin
        s1:=fopari(op,fcar(s));
        val(nameOf(s1),rop1,errCode1);
        s2:=fopari(op,fcdr(s));
        val(nameOf(s2),rop2,errCode2);
        if (errCode1<>0) or (errCode2<>0) then
          fopari:=ferreur(op,s)
        else
          begin
           case op of
             PLUS:testReal:=rop1 + rop2;
             MOINS:testReal:=rop1 - rop2;
             MULT:testReal:=rop1 * rop2;
             DIVIS: testReal:=rop1 / rop2;
           end;
           if (integerp(testReal, EPSILON)) then
             (* Convertir en entier *)
             str(trunc(testReal), resultat)
           else
             str(testReal, resultat);

           s1:=FINDSEXP(resultat);
           if not nullp(s1) then
             fopari:=s1
           else
             fopari:=NOUVATOM(oblist, resultat)^.SEXP;
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
(********* FONCTIONS MATHEMATIQUES ***********)
function fmath(func:string; smathentree:SGRAPHE): SGRAPHE;
var
  angle, mathval, mathvalbrute:real;
  resultat:string;
  errCode:integer;
  pMath:sgraphe;
begin
  if numberp(smathentree) or nullp(smathentree) then
    begin
      val(nameOf(smathentree), angle, errCode);
      mathvalbrute:=angle;
      angle:=degresEnRadians(angle);
      if errCode=0 then
        begin
        case func of
            _SIN: mathval:= sin(angle);
            _COS: mathval:= cos(angle);
            _TAN: mathval:= sin(angle)/cos(angle);
           _ATAN: mathval:= radiansEnDegres(arctan(mathvalbrute));
            _RAC: mathval:= sqrt(mathvalbrute);
            _ABS: mathval:= abs(mathvalbrute);
        end;
        if integerp(mathval, EPSILON) then
          str(trunc(mathval), resultat)
        else
          str(mathval, resultat);

        pMath:=FINDSEXP(resultat);
        if not nullp(pMath) then
             fmath:=pMath
           else
             fmath:=NOUVATOM(oblist, resultat)^.SEXP;
        end
      else
         ferreur(func+'-1', smathentree);
    end
  else
    fmath:= ferreur(func+'-2', smathentree);
end;
function fsin(degres:SGRAPHE): SGRAPHE;
begin
  fsin:=fmath(_SIN,degres);
end;
function fcos(degres:SGRAPHE): SGRAPHE;
begin
  fcos:=fmath(_COS,degres);
end;
function ftan(degres:SGRAPHE): SGRAPHE;
begin
  ftan:=fmath(_TAN,degres);
end;
function fatan(degres:SGRAPHE): SGRAPHE;
begin
  fatan:=fmath(_ATAN,degres);
end;

(* Affectation d'une variable *)
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
(*********** PROPERTY LIST *******************)
function fget(S:SGRAPHE):SGRAPHE;
var
  propname:SMALLSTRING;
  pprop:SGRAPHE;
  ppList:PTOBLIST;
begin
  if nullp(S) then
    fget:=NILE
  else
    begin
      ppList:=fcar(S)^.plist;
      (* Il faut sauter par-dessus l'atome QUOTE d'ou le FCDR supplementaire *)
      propname:=nameOf(fcar(fcdr(fcar(fcdr(S)))));
      pprop:=FINDSEXP2(propname, pplist);
      if pprop=NILE then
        fget:=NILE
      else
        fget:=pprop;
    end;
end;

function fsetf(pprop:SGRAPHE):sgraphe;
(* Syntaxe: (SETF (GET SYMBOL 'PROP) PROPVAL)*)
var
  propName: string;
  propVal, propExp, sexpProp, owner:SGRAPHE;

begin
  propExp:=fcar(pprop);
  owner:=fcar(fcdr(propExp));
  sexpProp:=eval(propExp);
  if nullp(sexpProp) then
    begin
      (* La propriete n'existe pas encore on la cree *)
      propname:=nameOf(fcar(fcdr(fcar(fcdr(fcdr(propExp))))));
      sexpProp:=NOUVATOM(owner^.plist, propname)^.SEXP;
    end;
  propVal:=valueOf(fcar(fcdr(pprop)));
  sexpProp^.val:=propVal;
  fsetf:=sexpProp;
end;
function fgetf(S:sgraphe):sgraphe;
(* Syntaxe: (GETF SYMBOL 'PROP) qui retourne PROPVAL *)
var
  propname:SMALLSTRING;
  pprop:SGRAPHE;
  ppList:PTOBLIST;
begin
  if nullp(S) then
    fgetf:=NILE
  else
    begin
      ppList:=fcar(S)^.plist;
      (* Il faut sauter par-dessus l'atome QUOTE d'ou le FCDR supplementaire *)
      propname:=nameOf(fcar(fcdr(fcar(fcdr(S)))));
      pprop:=FINDSEXP2(propname, pplist);
      if pprop=NILE then
        fgetf:=NILE
      else
        fgetf:=valueOf(pprop);
    end;
end;

procedure proplist(owner:sgraphe);
begin
  obprint1(owner^.plist);
end;

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
      BEGIN
        INTER[1]:= TAMPON;
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
        LUATOME:=FINDSEXP(LUCHAINE);
        (* C'EST UN NOUVEAU NOM -> ON L'ENREGISTRE *)
        IF nullp(LUATOME) THEN
          READATOM:=NOUVATOM(OBLIST, LUCHAINE)^.SEXP
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
              WRITE(nameOf(S),OBSEP)
           else
              WRITE(NUL,OBSEP);
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
                 END
              ELSE IF quotep(FCAR(S)) THEN
                BEGIN
                  (* TRANSFORMATION INVERSE DE LA LECTURE :
                  ((QUOTE) (ATOME)) => '(ATOME)
                  *)
                  WRITE(SPC,QT);
                  PRINT(FCAR(FCDR(S))); (* ON REPART COMME POUR UNE NOUVELLE LISTE *)
                END
              ELSE
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

(******** CHARGEMENT D'UN FICHIER OU LECTURE CONSOLE ****)
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
      WRITE(TAB,'APPLY:"');
      PRINT(FN);
      WRITE('"' +FD);
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
      IF nameOf(FN)='NEQ'       THEN APPLY:=FNEQ(FCAR(ARGS),FCAR(FCDR(ARGS))) ELSE
      IF nameOf(FN)='>'         THEN APPLY:=FGT(EVLIS(ARGS)) ELSE
      IF nameOf(FN)='>='         THEN APPLY:=FGE(EVLIS(ARGS)) ELSE
      IF nameOf(FN)='<'         THEN APPLY:=FLT(EVLIS(ARGS)) ELSE
      IF nameOf(FN)='<='         THEN APPLY:=FLE(EVLIS(ARGS)) ELSE
      IF nameOf(FN)=PLUS         THEN APPLY:=FADD(EVAL(FCONS(ZERO, ARGS))) ELSE
      IF nameOf(FN)=MOINS        THEN APPLY:=FSUB(EVAL(FCONS(ZERO, ARGS))) ELSE
      IF nameOf(FN)=MULT         THEN APPLY:=FMULT(EVAL(FCONS(UN, ARGS))) ELSE
      IF nameOf(FN)=DIVIS        THEN APPLY:=FDIV(EVAL(FCONS(UN, ARGS))) ELSE
      IF nameOf(FN)=_SIN         THEN APPLY:=FSIN(EVAL(FCAR((ARGS)))) ELSE
      IF nameOf(FN)=_COS         THEN APPLY:=FCOS(EVAL(FCAR((ARGS)))) ELSE
      IF nameOf(FN)=_TAN         THEN APPLY:=FTAN(EVAL(FCAR((ARGS)))) ELSE
      IF nameOf(FN)=_ATAN        THEN APPLY:=FATAN(EVAL(FCAR((ARGS)))) ELSE
      IF nameOf(FN)=_RAC         THEN APPLY:=FMATH(_RAC,EVAL(FCAR((ARGS)))) ELSE
      IF nameOf(FN)=_ABS         THEN APPLY:=FMATH(_ABS,EVAL(FCAR((ARGS)))) ELSE
      IF nameOf(FN)='NULL'      THEN APPLY:=FNULL(FCAR(ARGS)) ELSE
      IF nameOf(FN)='READ'      THEN APPLY:=FREAD(INPUT) ELSE
      IF nameOf(FN)='PRINT'     THEN BEGIN
                                        PRINT(FCAR(ARGS));
                                        APPLY:=NEANT;
                                     END ELSE
      IF nameOf(FN)='LOAD'      THEN BEGIN
                                       if (quotep(fcar(fcar(args)))) then
                                         apply:=fload(fcar(fcdr(fcar(args))))
                                       else
                                         APPLY:=FLOAD(FCAR(ARGS))
                                       END ELSE
      IF nameOf(FN)='EVAL'       THEN BEGIN
                                       if quotep(fcar(fcar(ARGS))) then
                                         APPLY:=EVAL(FCAR(FCDR(FCAR(ARGS))))
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
      WRITE(TAB,'EVAL:"');
      PRINT(E);
      WRITE('"');
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
                                      EVAL:=FINDSEXP('TRACE') END ELSE
      IF nameOf(S)='UNTRACE'    THEN BEGIN
                                      TRACE:=FALSE;
                                      (* Scan Page 43 Col. 2 *)
                                      EVAL:=FINDSEXP('UNTRACE') END ELSE
      IF nameOf(S)='SETQ'       THEN EVAL:=FSETQ(FCDR(E)) ELSE
      IF nameOf(S)='GET'       THEN EVAL:=FGET(FCDR(E)) ELSE
      IF nameOf(S)='GETF'       THEN EVAL:=FGETF(FCDR(E)) ELSE
      IF nameOf(S)='SETF'       THEN EVAL:=FSETF(FCDR(E)) ELSE
      IF nameOf(S)='PROPLIST'   THEN BEGIN EVAL:=NILE; PROPLIST(FCAR(FCDR(E))); END ELSE
      IF nameOf(S)='OBLIST'    THEN BEGIN
                                        EVAL:=NILE;
                                        OBPRINT;
                                     END ELSE
      IF nameOf(S)='QUIT'      THEN BEGIN
                                        EVAL:=NILE;
                                        FINSESS:=TRUE;
                                     END ELSE
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
      write(tab,'#');print(eval);writeln;
    end;
END;

(******* INITIALISATION ******)
PROCEDURE INIT;

VAR
  OBCOUR, PPCONSOLE:PTOBLIST;

BEGIN
  TRACE:=FALSE;
  str(PI, sPI);
  (* CREATION DU PREMIER ATOME *)
  NEW(NILE);
  NEW(NILE^.PNAME);
  NILE^.PNAME^:='()';
  NEW(OBCOUR);
  OBCOUR^.SEXP:=NILE;
  (* ON CONSERVE L'ENTREE DANS LA LISTE AVEC OBLIST *)
  OBLIST:=OBCOUR;
  OBCOUR:=NOUVATOM(OBCOUR, 'T'); TRU:=OBCOUR^.SEXP;
  OBCOUR:=NOUVATOM(OBCOUR, 'QUOTE'); AQUOTE:=OBCOUR^.SEXP;
  OBCOUR:=NOUVATOM(OBCOUR, 'CAR');
  OBCOUR:=NOUVATOM(OBCOUR, 'CDR');
  OBCOUR:=NOUVATOM(OBCOUR, 'CONS');
  OBCOUR:=NOUVATOM(OBCOUR, 'ATOM');
  OBCOUR:=NOUVATOM(OBCOUR, 'EQ');
  OBCOUR:=NOUVATOM(OBCOUR, 'LAMBDA'); LAMBDA:=OBCOUR^.SEXP;
  OBCOUR:=NOUVATOM(OBCOUR, 'READ');
  OBCOUR:=NOUVATOM(OBCOUR, 'PRINT');
  OBCOUR:=NOUVATOM(OBCOUR, 'COND');
  OBCOUR:=NOUVATOM(OBCOUR, 'TRACE');
  OBCOUR:=NOUVATOM(OBCOUR, 'UNTRACE');
  OBCOUR:=NOUVATOM(OBCOUR, 'SETQ');
  OBCOUR:=NOUVATOM(OBCOUR, 'LOAD');
  OBCOUR:=NOUVATOM(OBCOUR, 'OBLIST');
  OBCOUR:=NOUVATOM(OBCOUR, 'QUIT');
  OBCOUR:=NOUVATOM(OBCOUR, '0'); ZERO:=OBCOUR^.SEXP;
  OBCOUR:=NOUVATOM(OBCOUR, '1'); UN:=OBCOUR^.SEXP;
  OBCOUR:=NOUVATOM(OBCOUR, 'PI'); PPI:=OBCOUR^.SEXP;
  OBCOUR:=NOUVATOM(OBCOUR, sPI); PPI^.VAL:=OBCOUR^.SEXP;
  OBCOUR:=NOUVATOM(OBCOUR, VIDE); NEANT:=OBCOUR^.SEXP;

  TRU^.VAL:=TRU;
  NILE^.VAL:=NILE;
  NEW(PPCONSOLE);
  PPCONSOLE:=NOUVATOM(PPCONSOLE, 'CONSOLE');
  PCONSOLE:=PPCONSOLE^.SEXP;
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
