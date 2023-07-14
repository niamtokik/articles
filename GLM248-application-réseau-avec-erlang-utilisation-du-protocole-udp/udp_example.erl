%%%-------------------------------------------------------------------
%%% @author Mathieu Kerjouan
%%% @copyright 2021 (c) Mathieu Kerjouan
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(udp_example).
-export([dns_client/0]).
-include_lib("kernel/src/inet_dns.hrl").

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
dns_client() ->
    PropListEnTete = [{id, 1}         % identifiant de la requete DNS, normalement aléatoire 
                     ,{qr, false}     % est-ce que la requete est une réponse? 
                     ,{pr, false}     % est-ce qu’il est nécessaire d’avoir un serveur primaire? 
                     ,{rcode, 0}      % code de réponse 
                     ,{opcode, query} % type de message, query, iquery ou status
                     ,{aa, false}     % réponse de l’autorité
                     ,{tc, false}     % est-ce que le message est tronqué?
                     ,{rd, true}      % est-ce qu’une récursion est souhaité?
                     ,{ra, false}],   % est-ce que la récursion est disponible? 

    EnTete = inet_dns:make_header(PropListEnTete),

    Query = [{domain, "erlang-punch.eu"} % configuration du domaine désiré
            ,{type, a}                   % configuration du type d’enregistrement, A
            ,{class, in}],               % configuration de la classe, IN
    
    RequeteDNS = inet_dns:make_dns_query(Query),

    Filtre = [{domain, "."} % récupération du domaine racine uniquement
             ,{type, opt}], % utilisation d’une requete de type pseudo-RR (voir RFC2671)
    
    RequeteAdditionnelle = inet_dns:make_rr(Filtre),
    
    Requete = [{header, EnTete}                   % définis à l’étape 4
              ,{qdlist, [RequeteDNS]}             % définis à l’étape 6
              ,{arlist, [RequeteAdditionnelle]}], % définis à l’étape 8

    Message = inet_dns:make_msg(Requete),
    
    ChargeUtile = inet_dns:encode(Message),
    
    UDPSocketOptions = [{mode, binary}    % type de message attendu en émission et en retour
                       ,{active, false}], % mode de fonctionnement du socket

    {ok, Socket} = gen_udp:open(31415, UDPSocketOptions),
    
    ok = gen_udp:send(Socket, {80,67,169,12}, 53, ChargeUtile),
    
    {ok, {_SourceAdresse, _SourcePort, Reponse}} = gen_udp:recv(Socket, 1024, 1000),
    
    {ok, {dns_rec, _EnTete, _Requete, Resultat, _, _} = _ReponseDNS} = inet_dns:decode(Reponse),

    io:format("resultat: ~p~n", [Resultat]),
    
    ok = gen_udp:close(Socket).
