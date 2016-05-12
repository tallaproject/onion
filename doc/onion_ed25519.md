

# Module onion_ed25519 #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

Ed25519 API.

__Authors:__ Alexander Færøy ([`ahf@0x90.dk`](mailto:ahf@0x90.dk)).

<a name="types"></a>

## Data Types ##




### <a name="type-keypair">keypair()</a> ###


<pre><code>
keypair() = <a href="/Users/ahf/src/lab.baconsvin.org/talla/onion/_build/default/lib/ed25519_ref10/doc/ed25519_ref10.md#type-keypair">ed25519_ref10:keypair()</a>
</code></pre>




### <a name="type-public_key">public_key()</a> ###


<pre><code>
public_key() = <a href="/Users/ahf/src/lab.baconsvin.org/talla/onion/_build/default/lib/ed25519_ref10/doc/ed25519_ref10.md#type-public_key">ed25519_ref10:public_key()</a>
</code></pre>




### <a name="type-secret_key">secret_key()</a> ###


<pre><code>
secret_key() = <a href="/Users/ahf/src/lab.baconsvin.org/talla/onion/_build/default/lib/ed25519_ref10/doc/ed25519_ref10.md#type-secret_key">ed25519_ref10:secret_key()</a>
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#keypair-0">keypair/0</a></td><td>Creates a new Ed25519 keypair.</td></tr><tr><td valign="top"><a href="#keypair_from_x25519_keypair-1">keypair_from_x25519_keypair/1</a></td><td>Return the matching Ed25519 keypair from an x25519 keypair.</td></tr><tr><td valign="top"><a href="#open-3">open/3</a></td><td>Verify a Signature of a Message using the PublicKey.</td></tr><tr><td valign="top"><a href="#public_key_from_secret_key-1">public_key_from_secret_key/1</a></td><td>Compute the public key from a given secret key.</td></tr><tr><td valign="top"><a href="#public_key_from_x25519_public_key-2">public_key_from_x25519_public_key/2</a></td><td>Return the matching Ed25519 public key from an x25519 public key.</td></tr><tr><td valign="top"><a href="#secret_key_expand-1">secret_key_expand/1</a></td><td>Generate a new Ed25519 from a given seed.</td></tr><tr><td valign="top"><a href="#sign-2">sign/2</a></td><td>Sign a given Message using a given SecretKey.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="keypair-0"></a>

### keypair/0 ###

<pre><code>
keypair() -&gt; KeyPair
</code></pre>

<ul class="definitions"><li><code>KeyPair = <a href="#type-keypair">keypair()</a></code></li></ul>

Creates a new Ed25519 keypair.

Generates and returns a new Ed25519 keypair. The return value is a map to
avoid using the public key as the secret key and vice versa.

<a name="keypair_from_x25519_keypair-1"></a>

### keypair_from_x25519_keypair/1 ###

<pre><code>
keypair_from_x25519_keypair(X25519KeyPair) -&gt; {KeyPair, SignBit}
</code></pre>

<ul class="definitions"><li><code>X25519KeyPair = <a href="onion_x25519.md#type-keypair">onion_x25519:keypair()</a></code></li><li><code>KeyPair = <a href="#type-keypair">keypair()</a></code></li><li><code>SignBit = 0 | 1</code></li></ul>

Return the matching Ed25519 keypair from an x25519 keypair.

<a name="open-3"></a>

### open/3 ###

<pre><code>
open(Message, Signature, PublicKey) -&gt; boolean()
</code></pre>

<ul class="definitions"><li><code>Signature = binary()</code></li><li><code>Message = binary()</code></li><li><code>PublicKey = <a href="#type-public_key">public_key()</a></code></li></ul>

Verify a Signature of a Message using the PublicKey

Verifies a given Signature and Message using the given PublicKey.

<a name="public_key_from_secret_key-1"></a>

### public_key_from_secret_key/1 ###

<pre><code>
public_key_from_secret_key(SecretKey) -&gt; PublicKey
</code></pre>

<ul class="definitions"><li><code>SecretKey = <a href="#type-secret_key">secret_key()</a></code></li><li><code>PublicKey = <a href="#type-public_key">public_key()</a></code></li></ul>

Compute the public key from a given secret key.

Computes an Ed 25519 public key from a given secret key.

<a name="public_key_from_x25519_public_key-2"></a>

### public_key_from_x25519_public_key/2 ###

<pre><code>
public_key_from_x25519_public_key(X25519PublicKey, SignBit) -&gt; PublicKey
</code></pre>

<ul class="definitions"><li><code>X25519PublicKey = <a href="onion_x25519.md#type-public_key">onion_x25519:public_key()</a></code></li><li><code>SignBit = 0 | 1</code></li><li><code>PublicKey = <a href="#type-public_key">public_key()</a></code></li></ul>

Return the matching Ed25519 public key from an x25519 public key.

<a name="secret_key_expand-1"></a>

### secret_key_expand/1 ###

<pre><code>
secret_key_expand(Seed) -&gt; <a href="#type-secret_key">secret_key()</a>
</code></pre>

<ul class="definitions"><li><code>Seed = binary()</code></li></ul>

Generate a new Ed25519 from a given seed.

Generates and returns a new Ed25519 secret key.

<a name="sign-2"></a>

### sign/2 ###

<pre><code>
sign(Message, SecretKey) -&gt; Signature
</code></pre>

<ul class="definitions"><li><code>Message = iolist()</code></li><li><code>SecretKey = <a href="#type-secret_key">secret_key()</a></code></li><li><code>Signature = binary()</code></li></ul>

Sign a given Message using a given SecretKey

Returns a detached signature of a given Message using the given SecretKey.

