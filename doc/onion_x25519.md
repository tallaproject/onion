

# Module onion_x25519 #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

x25519 Diffie-Hellman API.

__Authors:__ Alexander Færøy ([`ahf@0x90.dk`](mailto:ahf@0x90.dk)).

<a name="types"></a>

## Data Types ##




### <a name="type-keypair">keypair()</a> ###


<pre><code>
keypair() = #{secret =&gt; <a href="#type-secret_key">secret_key()</a>, public =&gt; <a href="#type-public_key">public_key()</a>}
</code></pre>




### <a name="type-public_key">public_key()</a> ###


<pre><code>
public_key() = binary()
</code></pre>




### <a name="type-secret_key">secret_key()</a> ###


<pre><code>
secret_key() = binary()
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#keypair-0">keypair/0</a></td><td>Creates a new x25519 Diffie-Hellman keypair.</td></tr><tr><td valign="top"><a href="#scalarmult-2">scalarmult/2</a></td><td>Computes the scalar multiplication between SecretKey and BasePoint.</td></tr><tr><td valign="top"><a href="#secret_key_to_public_key-1">secret_key_to_public_key/1</a></td><td>Creates a PublicKey from a given SecretKey.</td></tr><tr><td valign="top"><a href="#shared_secret-2">shared_secret/2</a></td><td>Computes the shared secret between a SecretKey and PublicKey.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="keypair-0"></a>

### keypair/0 ###

<pre><code>
keypair() -&gt; KeyPair
</code></pre>

<ul class="definitions"><li><code>KeyPair = <a href="#type-keypair">keypair()</a></code></li></ul>

Creates a new x25519 Diffie-Hellman keypair.

Generates and returns a new x25519 Diffie-Hellman keypair. The return value
is a map to avoid using the public key as the secret key and vice versa.

<a name="scalarmult-2"></a>

### scalarmult/2 ###

<pre><code>
scalarmult(SecretKey, BasePoint) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>SecretKey = <a href="#type-secret_key">secret_key()</a></code></li><li><code>BasePoint = binary()</code></li><li><code>Result = binary()</code></li></ul>

Computes the scalar multiplication between SecretKey and BasePoint.

This function computes the scalar multiplication between a given SecretKey
and a given BasePoint.

<a name="secret_key_to_public_key-1"></a>

### secret_key_to_public_key/1 ###

<pre><code>
secret_key_to_public_key(SecretKey) -&gt; PublicKey
</code></pre>

<ul class="definitions"><li><code>SecretKey = <a href="#type-secret_key">secret_key()</a></code></li><li><code>PublicKey = <a href="#type-public_key">public_key()</a></code></li></ul>

Creates a PublicKey from a given SecretKey.

This function creates an x25519 public key from a given x25519 secret key.

<a name="shared_secret-2"></a>

### shared_secret/2 ###

<pre><code>
shared_secret(SecretKey, PublicKey) -&gt; SharedSecret
</code></pre>

<ul class="definitions"><li><code>SecretKey = <a href="#type-secret_key">secret_key()</a></code></li><li><code>PublicKey = <a href="#type-public_key">public_key()</a></code></li><li><code>SharedSecret = binary()</code></li></ul>

Computes the shared secret between a SecretKey and PublicKey.

This function computes the shared secret between a given SecretKey and
PublicKey.

