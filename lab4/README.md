# P2P сервис обмена сообщениями с шифрованием

- Студенты: `Исупов Никита Александрович` , `Зонов Николай Андреевич`
- Язык: `Erlang`
---

# Ключевые моменты реализации

## p2p_node.erl
Модуль `p2p_node` реализует центральную ноду приложения и отвечает за:

- запуск TCP-сервера;
- приём входящих соединений;
- установку исходящих соединений;
- хранение активных пиров;
- маршрутизацию входящих сообщений;
- сохранение истории переписки.

Нода реализована как `gen_server`.

Приём входящих соединений и создание процесса соединения:

```erlang 
acceptor(Listen, Node) ->
    case gen_tcp:accept(Listen) of
        {ok, Sock} ->
            {ok, Conn} = p2p_conn:start_link(Sock, Node),
            gen_tcp:controlling_process(Sock, Conn),
            gen_server:cast(Conn, init_socket),
            Node ! {new_connection, Conn, Sock},
            acceptor(Listen, Node);
        _ ->
            timer:sleep(1000),
            acceptor(Listen, Node)
    end.
```

Обработка входящих зашифрованных сообщений от `p2p_conn`:

```erlang
handle_info({incoming_msg, Peer, Plain}, State) ->
    Ts = calendar:system_time_to_rfc3339(
        erlang:system_time(seconds), [{unit, second}]
    ),
    p2p_history:persist({self(), Peer, Ts, incoming, Plain}),
    io:format("<< [~s] ~p: ~s~n", [Ts, Peer, Plain]),
    {noreply, State};
```

## p2p_conn.erl
Модуль для установления соединений и обмена зашифрованными сообщениями, отвечает за handshake и очередь сообщений.

## p2p_crypto.erl
Модуль генерации ключей и шифрования

Генерация пары ключей:

```erlang
generate_keypair() ->
    crypto:generate_key(ecdh, secp256r1).
```

Вычисление общего секрета:
```erlang
compute_shared_key(TheirPubBin, MyPriv) ->
    TheirPub = binary_to_term(TheirPubBin),
    Shared =
        crypto:compute_key(
            ecdh, TheirPub, MyPriv, secp256r1
        ),
    crypto:hash(sha256, Shared).
```
Само шифрование 

```erlang 
encrypt(Key, Plain) ->
    IV = crypto:strong_rand_bytes(12),
    {Cipher, Tag} =
        crypto:crypto_one_time_aead(
            aes_256_gcm, Key, IV, Plain, <<>>, true
        ),
    <<IV/binary, Tag/binary, Cipher/binary>>.
```

## p2p_cli.erl
Модуль CLI для взаимодействия с юзером

## p2p_history.erl
Модуль хранения истории сообщений (локально)

Запись:

```erlang 
persist(Message) ->
    Bin = term_to_binary(Message),
    Len = byte_size(Bin),
    {ok, File} = file:open("history.dat", [append, {raw, true}]),
    file:write(File, <<Len:32/big, Bin/binary>>),
    file:close(File).
```
Загрузка:

```erlang 
load_all() ->
    case file:read_file("history.dat") of
        {ok, Bin} -> load_all_from_bin(Bin, []);
        _ -> []
    end.
```

## Запуск

```bash
escript p2p_cli.erl **port**
```
Соединение 
```bash
> connect 127.0.0.1 5000
```

Отправка

```bash
>send localhost 5001 hello
```

Пример получаемого сообщения

```bash
<< [2025-12-16T16:23:26+03:00] {{{127,0,0,1},54132},<0.78.0>}: hello
```
