

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


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#keypair-0">keypair/0</a></td><td>Creates a new Ed25519 keypair.</td></tr><tr><td valign="top"><a href="#sign-2">sign/2</a></td><td>Sign a given Message using a given SecretKey.</td></tr><tr><td valign="top"><a href="#verify-3">verify/3</a></td><td>Verify a Signature of a Message using the PublicKey.</td></tr></table>


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

<a name="sign-2"></a>

### sign/2 ###

<pre><code>
sign(Message, SecretKey) -&gt; Signature
</code></pre>

<ul class="definitions"><li><code>Message = iolist()</code></li><li><code>SecretKey = <a href="#type-secret_key">secret_key()</a></code></li><li><code>Signature = binary()</code></li></ul>

Sign a given Message using a given SecretKey

Returns a detached signature of a given Message using the given SecretKey.

<a name="verify-3"></a>

### verify/3 ###

<pre><code>
verify(Message, Signature, PublicKey) -&gt; boolean()
</code></pre>

<ul class="definitions"><li><code>Signature = binary()</code></li><li><code>Message = binary()</code></li><li><code>PublicKey = <a href="#type-public_key">public_key()</a></code></li></ul>

Verify a Signature of a Message using the PublicKey

Verifies a given Signature and Message using the given PublicKey.

