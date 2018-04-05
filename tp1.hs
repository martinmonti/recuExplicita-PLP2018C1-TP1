import Data.Maybe
import Test.HUnit
import MultiDict


--Multidiccionarios de prueba

infinito :: MultiDict Integer Integer
infinito = tablas 1

cuatroNiveles :: MultiDict Integer Char
cuatroNiveles = Entry 1 'a' (Multi 2 (Entry 2 'b' (Multi 3 (Entry 3 'c' (Multi 4 (Entry 4 'd' Nil) Nil)) Nil)) Nil)

superinfinito :: MultiDict Integer Integer
superinfinito = Multi 1 (Entry 1 1 superinfinito) (mapMD (+1) (*2) superinfinito)

datosLlamada :: MultiDict String String --Cualquier parecido con los datos de una llamada que hizo un cliente a un banco son absolutamente...
datosLlamada = 
    Entry "EventSequenceNumber" "21866615" $
    Entry "Time" "1521555643436" $
    Multi "Extensions"
       ( Entry "parent-party-uuid" "OQG87JKNJT4HJ568BGVTLHBSES07S0NU" $
         Entry "OtherTrunkName" "HiPath8000_Trunk" $
         Entry "OriginationDN" "3584622309"$
         Entry "OriginationDN_location" "SipSwitch_BHSA"$
         Entry "BusinessCall" "1" $
         Entry "NO_ANSWER_TIMEOUT" "30" $
         Entry "NO_ANSWER_ACTION" "notready" $
         Entry "NO_ANSWER_OVERFLOW" "recall" $ Nil) $
    Entry "PartyUuid" "OQG87JKNJT4HJ568BGVTLHBSES07S1EH" $
    Entry "ThirdPartyQueue" "1665" $
    Entry "OtherQueue" "3013" $
    Entry "ThisQueue" "44303" $
    Entry "ThisDNRole" "2 [RoleDestination]" $
    Entry "AgentID" "1364" $
    Entry "ThisDN" "541143356512" $
    Entry "ANI" "3584622309" $
    Entry "DNIS" "1665" $
    Multi "UserData"
       ( Entry "Preatendedor" "Si" $
         Multi "RRequestedSkills" Nil $
         Entry "CustomerSegment" "default" $
         Entry "ServiceType" "default" $
         Entry "ServiceObjective" "" $
         Entry "scriptidkeyname" "1665" $
         Entry "Sucursal" "RIO CUARTO" $
         Entry "TOOLBAR_REAL" "Totem RIO CUARTO" $
         Entry "GVP_CID" "000202ba29a61ad6" $
         Entry "Ambiente" "PROD" $
         Entry "Contingencia" "0" $
         Entry "CrearClave" "ON" $
         Entry "ANI" "3584622309" $
         Entry "GVP_FIN" "B3368B05CE6941F5362536DC59139BDFB8FEF4E81EB41562BF4579BA55F824DD254F9187366F7D7A4139273BC5DCA95DC8D1255E6F0BEF0902E32AC70E8EDC87DB7B11F555D8A3E80B299EEB" $
         Entry "GVP_GRUPO" "TOTEM" $
         Entry "GVP_INICIO" "9B22D23EF36D1237A5AC20EE3D8CF304F872B2018DDC09CD956E28107B5EA7215212436508360E550B9F07499B608DA76553D1AD25CDE5C574C981450F883C2BC8FE8E2209FEAB31B7C123FE00BA3F6D1DB9E9B8EC62D218620BA9B6620104E39AFC4177" $
         Entry "GVP_MSJULTIMOACCESO" "TOTEM - Desbloquear Clave" $
         Entry "GVP_NRODOC" "12599644" $
         Entry "GVP_URL" "http://canalesredirector/prisma/desbloquearPIN.do" $
         Entry "destino" "sip:44303@192.168.8.61" $
         Entry "documento" "12599644" $
         Entry "URL1" "http://canalesredirector/prisma/desbloquearPIN.do" $
         Entry "datosLlamada" "9B22D23EF36D1237A5AC20EE3D8CF304F872B2018DDC09CD956E28107B5EA7215212436508360E550B9F07499B608DA76553D1AD25CDE5C574C981450F883C2BC8FE8E2209FEAB31B7C123FE00BA3F6D1DB9E9B8EC62D218620BA9B6620104E39AFC4177" $
         Entry "datosLlamada2" "B3368B05CE6941F5362536DC59139BDFB8FEF4E81EB41562BF4579BA55F824DD254F9187366F7D7A4139273BC5DCA95DC8D1255E6F0BEF0902E32AC70E8EDC87DB7B11F555D8A3E80B299EEB" $
         Entry "documento_cliente" "12599644" $
         Entry "SecsInACWToReadyInbound" "15" $
         Entry "AUTOATENCION" "SI" $
         Entry "StatusAfterInbound" "Amarillo" $
         Entry "skill" "" $
         Entry "skillTotem" "" $
         Entry "ColaTotem" "SI" $
         Entry "Browser" "1" $
         Entry "Grupo_Asesor" "Totem" $
         Entry "IW_OverrideOptions" "CUS_Inbound" $
         Entry "RPVQID" "0FPCKSKVOL4GP2PSNV0B7OBN4400SHR8" $
         Entry "RTargetAgentGroup" "?:DesbordeTotem = 1" $
         Entry "RTargetAgentGroup" "?:Totem = 1" $
         Entry "RVQID" "0FPCKSKVOL4GP2PSNV0B7OBN4400SHR8" $
         Entry "RVQDBID" "1523" $
         Entry "RTargetTypeSelected" "2" $
         Entry "RTargetRuleSelected" "" $
         Entry "RTargetObjectSelected" "?:Totem = 1" $
         Entry "RTargetObjSelDBID" "" $
         Entry "RTargetAgentSelected" "MRomero" $
         Entry "RTargetAgSelDBID" "1812" $
         Entry "RTargetPlaceSelected" "P_6512" $
         Entry "RTargetPlSelDBID" "616" $
         Entry "RTenant" "Hipotecario" $
         Entry "RTenantDBID" "103" $
         Entry "RStrategyName" "Segmentacion_Totem_filtrada" $
         Entry "RStrategyDBID" "308" $
         Entry "CBR-actual_volume" "" $
         Entry "CBR-Interaction_cost" "" $
         Entry "CBR-contract_DBIDs" "" $
         Entry "CBR-IT-path_DBIDs" "" $
         Entry "RRequestedSkillCombination" "" $
         Entry "RTargetRequested" "?:Totem = 1" $
         Entry "PegAG?:Totem = 1" "1" $
         Entry "PegTD" "4" $
         Entry "PegDOW" "4" Nil ) $
    Entry "CallUuid" "2U6F4SQ93170BADU8LSQ871KB8012UN7" $
    Entry "ConnID" "000202BA29A61AD6" $
    Entry "CallID" "17922791" $
    Entry "PropagatedCallType" "2 [Inbound]" $
    Entry "CallType" "2 [Inbound]" $
    Entry "CallState" "0" $
    Entry "OtherDNRole" "1 [RoleOrigination]" $
    Entry "OtherDN" "3584622309" Nil

    
--Ejecución de los tests
main :: IO Counts
main = do runTestTT allTests

allTests = test [
  "ejercicio1a" ~: tests2,
  "ejercicio1b" ~: tests2,
  "ejercicio2a" ~: tests3,
  "ejercicio2b" ~: tests4,
  "ejercicio3" ~: tests5,
  "ejercicio4" ~: tests6,
  "ejercicio5a" ~: tests7,
  "ejercicio5b" ~: tests8,
  "ejercicio6" ~: tests9,
  "ejercicio7" ~: tests10
  ]

tests1 = test [
   podar 5 5 cuatroNiveles ~=? cuatroNiveles
  ]
tests2 = test [
   recMD 0 (\_ _ _ ->id) (\_ m1 _ r1 r2->if isNil m1 then r2 else 1+r1+r2) cuatroNiveles ~=? 3
  ]
tests3 = test [
   profundidad cuatroNiveles ~=? 4,
   profundidad datosLlamada ~=? 2
  ]
tests4 = test [
   tamaño cuatroNiveles ~=? 7,
   (tamaño $ podar 5 3 datosLlamada) ~=? 10,
   tamaño datosLlamada ~=? 89
  ]
tests5 = test [
   (podar 5 3 $ tablas 2) ~=? (podar 5 2 $ tablas 2),
   (profundidad $ podar 20 20 infinito) ~=? 2,
   (tamaño $ podar 5 3 $ tablas 2) ~=? 30
  ]
tests6 = test [
  serialize cuatroNiveles ~=? "[1: 'a', [2: [2: 'b', [3: [3: 'c', [4: [4: 'd', [ ]], [ ]]], [ ]]], [ ]]]"
  ]
tests7 = test [
   (profundidad $ podar 10 5 superinfinito) ~=? 5,
   (profundidad $ podar 3 10 superinfinito) ~=? 10,
   (tamaño $ podar 10 5 superinfinito) ~=? 82010
  ]
tests8 = test [
   (serialize $ filterMD even $ Entry 4 'e' cuatroNiveles) ~=? "[4: 'e', [2: [2: 'b', [ ]], [ ]]]",
   (serialize $ enLexicon ["extensions", "originationdn", "no_answer_action", "ani", "dnis", "userdata", "customersegment", "statusafterinbound", "connid"] datosLlamada) ~=? "[\"extensions\": [\"originationdn\": \"3584622309\", [\"no_answer_action\": \"notready\", [ ]]], [\"ani\": \"3584622309\", [\"dnis\": \"1665\", [\"userdata\": [\"customersegment\": \"default\", [\"ani\": \"3584622309\", [\"statusafterinbound\": \"Amarillo\", [ ]]]], [\"connid\": \"000202BA29A61AD6\", [ ]]]]]]"
  ]
tests9 = test [
   (serialize $ definir [2,3] 'c' cuatroNiveles) ~=? "[1: 'a', [2: [2: 'b', [3: 'c', [ ]]], [ ]]]",
   (profundidad $ definir [4] 'g' $ definir [4] 'e' $ definir [2,4] 'f' $ definir [2,3,4,5] 'e' cuatroNiveles) ~=? 4
  ]
tests10 = test [
   (obtener [4] $ definir [4] 'g' $ definir [4] 'e' $ definir [2,4] 'f' $ definir [2,3,4,5] 'e' cuatroNiveles) ~=? Just 'g',
   obtener [2,3,3] cuatroNiveles ~=? Just 'c',
   obtener [2,3,5] cuatroNiveles ~=? Nothing,
   obtener [6,7] infinito ~=? Just 42
  ]
