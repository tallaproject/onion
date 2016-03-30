

# Module onion_dh #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

Diffie-Hellman API.

__Authors:__ Alexander Færøy ([`ahf@0x90.dk`](mailto:ahf@0x90.dk)).

<a name="description"></a>

## Description ##

This module contains functions for key generation and shared secret
computations for the Diffie-Hellman key exchange algorithm.

Tor uses a generator (g) of 2 and the modulus (p) is a 1024-bit safe prime
from RFC 2409 section 6.2. See the Tor specification for more information.

<a name="types"></a>

## Data Types ##




### <a name="type-keypair">keypair()</a> ###


<pre><code>
keypair() = #{secret =&gt; <a href="#type-secret_key">secret_key()</a>, public =&gt; <a href="#type-public_key">public_key()</a>}
</code></pre>




### <a name="type-public_key">public_key()</a> ###


<pre><code>
public_key() = <a href="crypto.md#type-dh_public">crypto:dh_public()</a>
</code></pre>




### <a name="type-secret_key">secret_key()</a> ###


<pre><code>
secret_key() = <a href="crypto.md#type-dh_private">crypto:dh_private()</a>
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#is_degenerate-1">is_degenerate/1</a></td><td>Verify if a given integer is degenerate.</td></tr><tr><td valign="top"><a href="#keypair-0">keypair/0</a></td><td>Creates a new Diffie-Hellman keypair.</td></tr><tr><td valign="top"><a href="#params-0">params/0</a></td><td>Diffie-Hellman parameters used by Tor (from RFC 2409).</td></tr><tr><td valign="top"><a href="#shared_secret-2">shared_secret/2</a></td><td>Computes the shared secret between a SecretKey and PublicKey.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="is_degenerate-1"></a>

### is_degenerate/1 ###

<pre><code>
is_degenerate(Value) -&gt; boolean()
</code></pre>

<ul class="definitions"><li><code>Value = non_neg_integer()</code></li></ul>

Verify if a given integer is degenerate.

This function can be used to check that the g^x or g^x value is not
degenerate. That is, the value is within the range 2 and P - 2 (both
included).

<a name="keypair-0"></a>

### keypair/0 ###

<pre><code>
keypair() -&gt; <a href="#type-keypair">keypair()</a>
</code></pre>
<br />

Creates a new Diffie-Hellman keypair.

Generates and returns a new Diffie-Hellman keypair. The return value is a
map to avoid using the public key as the secret key and vice versa.

<a name="params-0"></a>

### params/0 ###

<pre><code>
params() -&gt; [non_neg_integer()]
</code></pre>
<br />

Diffie-Hellman parameters used by Tor (from RFC 2409).

This function will return a list containing two elements: P (the modulus)
and G (the generator). These parameters are needed for the other
Diffie-Hellman computations in this module and in the OTP crypto module.

<a name="shared_secret-2"></a>

### shared_secret/2 ###

<pre><code>
shared_secret(SecretKey, PublicKey) -&gt; {ok, SharedSecret} | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>SecretKey = <a href="#type-secret_key">secret_key()</a></code></li><li><code>PublicKey = <a href="#type-public_key">public_key()</a></code></li><li><code>SharedSecret = binary()</code></li><li><code>Reason = term()</code></li></ul>

Computes the shared secret between a SecretKey and PublicKey.

This function computes the shared secret between a given SecretKey and
PublicKey.

