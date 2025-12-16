-module(p2p_crypto).
-export([
    generate_keypair/0,
    pubkey_to_bin/1,
    compute_shared_key/2,
    encrypt/2,
    decrypt/2
]).

generate_keypair() ->
    crypto:generate_key(ecdh, secp256r1).

pubkey_to_bin(Pub) ->
    term_to_binary(Pub).

compute_shared_key(TheirPubBin, MyPriv) ->
    TheirPub = binary_to_term(TheirPubBin),
    Shared = crypto:compute_key(ecdh, TheirPub, MyPriv, secp256r1),
    crypto:hash(sha256, Shared).

encrypt(Key, Plain) ->
    IV = crypto:strong_rand_bytes(12),
    {Cipher, Tag} =
        crypto:crypto_one_time_aead(aes_256_gcm, Key, IV, Plain, <<>>, true),
    <<IV/binary, Tag/binary, Cipher/binary>>.

decrypt(Key, <<IV:12/binary, Tag:16/binary, Cipher/binary>>) ->
    case
        crypto:crypto_one_time_aead(
            aes_256_gcm, Key, IV, Cipher, <<>>, Tag, false
        )
    of
        error -> {error, bad_tag};
        Plain -> {ok, Plain}
    end;
decrypt(_, _) ->
    {error, bad_format}.
