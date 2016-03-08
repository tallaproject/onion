

# Module onion_ntor #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

NTor Handshake.

__Authors:__ Alexander Færøy ([`ahf@0x90.dk`](mailto:ahf@0x90.dk)).

<a name="description"></a>

## Description ##
For information about the NTor handshake, see tor-spec.txt, section 5.1.4.
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#client_handshake-5">client_handshake/5</a></td><td>Compute the shared secret.</td></tr><tr><td valign="top"><a href="#create-2">create/2</a></td><td>Create the client data to begin an NTor handshake.</td></tr><tr><td valign="top"><a href="#create-3">create/3</a></td><td>Create the client data to begin an NTor handshake.</td></tr><tr><td valign="top"><a href="#server_handshake-4">server_handshake/4</a></td><td>Compute shared secret and client response from initial handshake message from the client.</td></tr><tr><td valign="top"><a href="#server_handshake-5">server_handshake/5</a></td><td>Compute shared secret and client response from initial handshake message from the client.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="client_handshake-5"></a>

### client_handshake/5 ###

<pre><code>
client_handshake(ServerIDKeyHash, ServerNTorOnionPublicKey, ServerEphemeralPublicKey, ClientEphemeralKeyPair, Length) -&gt; {Auth, SharedSecret}
</code></pre>

<ul class="definitions"><li><code>ServerIDKeyHash = binary()</code></li><li><code>ServerNTorOnionPublicKey = <a href="onion_x25519.md#type-public_key">onion_x25519:public_key()</a></code></li><li><code>ServerEphemeralPublicKey = <a href="onion_x25519.md#type-public_key">onion_x25519:public_key()</a></code></li><li><code>ClientEphemeralKeyPair = <a href="onion_x25519.md#type-keypair">onion_x25519:keypair()</a></code></li><li><code>Length = non_neg_integer()</code></li><li><code>Auth = binary()</code></li><li><code>SharedSecret = binary()</code></li></ul>

Compute the shared secret.

This function computes the shared secret from the server's handshake
response. A client should verify that the Auth matches the AuthData from the
message that was send from the server to finalize the handshake.

<a name="create-2"></a>

### create/2 ###

<pre><code>
create(ServerIDKeyHash, ServerNTorPublicKey) -&gt; {Data, EphemeralKeyPair}
</code></pre>

<ul class="definitions"><li><code>ServerIDKeyHash = binary()</code></li><li><code>ServerNTorPublicKey = <a href="onion_x25519.md#type-public_key">onion_x25519:public_key()</a></code></li><li><code>Data = binary()</code></li><li><code>EphemeralKeyPair = <a href="onion_x25519.md#type-keypair">onion_x25519:keypair()</a></code></li></ul>

Create the client data to begin an NTor handshake.

This function returns the data that a client must send to a server to
initialize a NTor handshake. The function also returns a generated ephemeral
key pair. The ephemeral key pair must be stored to be able to handle the
response from the server handshake.

<a name="create-3"></a>

### create/3 ###

<pre><code>
create(ServerIDKeyHash, ServerNTorPublicKey, ClientEphemeralKeyPair) -&gt; {Data, ClientEphemeralKeyPair}
</code></pre>

<ul class="definitions"><li><code>ServerIDKeyHash = binary()</code></li><li><code>ServerNTorPublicKey = <a href="onion_x25519.md#type-public_key">onion_x25519:public_key()</a></code></li><li><code>ClientEphemeralKeyPair = <a href="onion_x25519.md#type-keypair">onion_x25519:keypair()</a></code></li><li><code>Data = binary()</code></li></ul>

Create the client data to begin an NTor handshake.

This function returns the data that a client must send to a server to
initialize a NTor handshake. The function also returns a generated ephemeral
key pair. The ephemeral key pair must be stored to be able to handle the
response from the server handshake.

This function takes the ClientEphemeralKeyPair as an argument, which allows
us to test the function in a deterministic way.

<a name="server_handshake-4"></a>

### server_handshake/4 ###

<pre><code>
server_handshake(ServerIDKeyHash, ServerNTorOnionKeyPair, ClientEphemeralPublicKey, Length) -&gt; {Data, Key}
</code></pre>

<ul class="definitions"><li><code>ServerIDKeyHash = binary()</code></li><li><code>ServerNTorOnionKeyPair = <a href="onion_x25519.md#type-keypair">onion_x25519:keypair()</a></code></li><li><code>ClientEphemeralPublicKey = <a href="onion_x25519.md#type-public_key">onion_x25519:public_key()</a></code></li><li><code>Length = non_neg_integer()</code></li><li><code>Data = binary()</code></li><li><code>Key = binary()</code></li></ul>

Compute shared secret and client response from initial handshake message from the client.

This function computes the shared secret and the message that must be send
to the client for the client to finalize the handshake.

<a name="server_handshake-5"></a>

### server_handshake/5 ###

<pre><code>
server_handshake(ServerIDKeyHash, ServerNTorOnionKeyPair, ClientEphemeralPublicKey, Length, ServerEphemeralKeyPair) -&gt; {Response, SharedSecret}
</code></pre>

<ul class="definitions"><li><code>ServerIDKeyHash = binary()</code></li><li><code>ServerNTorOnionKeyPair = <a href="onion_x25519.md#type-keypair">onion_x25519:keypair()</a></code></li><li><code>ClientEphemeralPublicKey = <a href="onion_x25519.md#type-public_key">onion_x25519:public_key()</a></code></li><li><code>ServerEphemeralKeyPair = <a href="onion_x25519.md#type-keypair">onion_x25519:keypair()</a></code></li><li><code>Length = non_neg_integer()</code></li><li><code>Response = binary()</code></li><li><code>SharedSecret = binary()</code></li></ul>

Compute shared secret and client response from initial handshake message from the client.

This function computes the shared secret and the message that must be send
to the client for the client to finalize the handshake.

This function takes the ServerEphemeralKeyPair as an argument, which allows
us to test the function in a deterministic way.

