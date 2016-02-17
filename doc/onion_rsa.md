

# Module onion_rsa #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

RSA Cryptosystem API.

__Authors:__ Alexander Færøy ([`ahf@0x90.dk`](mailto:ahf@0x90.dk)).

<a name="description"></a>

## Description ##
This helper module contains utilities for working with the RSA
cryptosystem. It's made to simplify the usage of Erlang's public_key
and crypto applications for our specific use case.

<a name="types"></a>

## Data Types ##




### <a name="type-key">key()</a> ###


<pre><code>
key() = <a href="#type-secret_key">secret_key()</a> | <a href="#type-public_key">public_key()</a>
</code></pre>




### <a name="type-keypair">keypair()</a> ###


<pre><code>
keypair() = #{secret =&gt; <a href="#type-secret_key">secret_key()</a>, public =&gt; <a href="#type-public_key">public_key()</a>}
</code></pre>




### <a name="type-public_key">public_key()</a> ###


<pre><code>
public_key() = <a href="public_key.md#type-rsa_public_key">public_key:rsa_public_key()</a>
</code></pre>




### <a name="type-secret_key">secret_key()</a> ###


<pre><code>
secret_key() = <a href="public_key.md#type-rsa_private_key">public_key:rsa_private_key()</a>
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#der_decode_public_key-1">der_decode_public_key/1</a></td><td></td></tr><tr><td valign="top"><a href="#der_decode_secret_key-1">der_decode_secret_key/1</a></td><td></td></tr><tr><td valign="top"><a href="#der_encode-1">der_encode/1</a></td><td></td></tr><tr><td valign="top"><a href="#key_size-1">key_size/1</a></td><td></td></tr><tr><td valign="top"><a href="#keypair-1">keypair/1</a></td><td></td></tr><tr><td valign="top"><a href="#keypair-2">keypair/2</a></td><td></td></tr><tr><td valign="top"><a href="#pem_decode-1">pem_decode/1</a></td><td></td></tr><tr><td valign="top"><a href="#pem_encode-1">pem_encode/1</a></td><td></td></tr><tr><td valign="top"><a href="#private_decrypt-2">private_decrypt/2</a></td><td></td></tr><tr><td valign="top"><a href="#private_decrypt-3">private_decrypt/3</a></td><td></td></tr><tr><td valign="top"><a href="#private_encrypt-2">private_encrypt/2</a></td><td></td></tr><tr><td valign="top"><a href="#private_encrypt-3">private_encrypt/3</a></td><td></td></tr><tr><td valign="top"><a href="#public_decrypt-2">public_decrypt/2</a></td><td></td></tr><tr><td valign="top"><a href="#public_decrypt-3">public_decrypt/3</a></td><td></td></tr><tr><td valign="top"><a href="#public_encrypt-2">public_encrypt/2</a></td><td></td></tr><tr><td valign="top"><a href="#public_encrypt-3">public_encrypt/3</a></td><td></td></tr><tr><td valign="top"><a href="#secret_key_to_public_key-1">secret_key_to_public_key/1</a></td><td></td></tr><tr><td valign="top"><a href="#sign-3">sign/3</a></td><td></td></tr><tr><td valign="top"><a href="#verify-4">verify/4</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="der_decode_public_key-1"></a>

### der_decode_public_key/1 ###

<pre><code>
der_decode_public_key(Bytes) -&gt; {ok, PublicKey} | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>Bytes = binary()</code></li><li><code>PublicKey = <a href="#type-public_key">public_key()</a></code></li><li><code>Reason = term()</code></li></ul>

<a name="der_decode_secret_key-1"></a>

### der_decode_secret_key/1 ###

<pre><code>
der_decode_secret_key(Bytes) -&gt; {ok, SecretKey} | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>Bytes = binary()</code></li><li><code>SecretKey = <a href="#type-secret_key">secret_key()</a></code></li><li><code>Reason = term()</code></li></ul>

<a name="der_encode-1"></a>

### der_encode/1 ###

<pre><code>
der_encode(Key) -&gt; {ok, Bytes} | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>Key = <a href="#type-key">key()</a></code></li><li><code>Bytes = binary()</code></li><li><code>Reason = term()</code></li></ul>

<a name="key_size-1"></a>

### key_size/1 ###

<pre><code>
key_size(Key) -&gt; BitSize
</code></pre>

<ul class="definitions"><li><code>Key = <a href="#type-key">key()</a></code></li><li><code>BitSize = non_neg_integer()</code></li></ul>

<a name="keypair-1"></a>

### keypair/1 ###

<pre><code>
keypair(Bits) -&gt; {ok, KeyPair} | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>Bits = pos_integer()</code></li><li><code>KeyPair = <a href="#type-keypair">keypair()</a></code></li><li><code>Reason = term()</code></li></ul>

<a name="keypair-2"></a>

### keypair/2 ###

<pre><code>
keypair(Bits, PublicExponent) -&gt; {ok, KeyPair} | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>Bits = pos_integer()</code></li><li><code>PublicExponent = pos_integer()</code></li><li><code>KeyPair = <a href="#type-keypair">keypair()</a></code></li><li><code>Reason = term()</code></li></ul>

<a name="pem_decode-1"></a>

### pem_decode/1 ###

<pre><code>
pem_decode(Bytes) -&gt; {ok, Key} | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>Bytes = binary()</code></li><li><code>Key = <a href="#type-key">key()</a></code></li><li><code>Reason = term()</code></li></ul>

<a name="pem_encode-1"></a>

### pem_encode/1 ###

<pre><code>
pem_encode(Key) -&gt; {ok, Bytes} | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>Key = <a href="#type-key">key()</a></code></li><li><code>Bytes = binary()</code></li><li><code>Reason = term()</code></li></ul>

<a name="private_decrypt-2"></a>

### private_decrypt/2 ###

<pre><code>
private_decrypt(CipherText, SecretKey) -&gt; PlainText
</code></pre>

<ul class="definitions"><li><code>CipherText = binary()</code></li><li><code>SecretKey = <a href="#type-secret_key">secret_key()</a></code></li><li><code>PlainText = binary()</code></li></ul>

<a name="private_decrypt-3"></a>

### private_decrypt/3 ###

<pre><code>
private_decrypt(CipherText, SecretKey, Padding) -&gt; PlainText
</code></pre>

<ul class="definitions"><li><code>CipherText = binary()</code></li><li><code>SecretKey = <a href="#type-secret_key">secret_key()</a></code></li><li><code>Padding = rsa_pkcs1_padding | rsa_pkcs1_oaep_padding | rsa_no_padding</code></li><li><code>PlainText = binary()</code></li></ul>

<a name="private_encrypt-2"></a>

### private_encrypt/2 ###

<pre><code>
private_encrypt(PlainText, SecretKey) -&gt; CipherText
</code></pre>

<ul class="definitions"><li><code>PlainText = binary()</code></li><li><code>SecretKey = <a href="#type-secret_key">secret_key()</a></code></li><li><code>CipherText = binary()</code></li></ul>

<a name="private_encrypt-3"></a>

### private_encrypt/3 ###

<pre><code>
private_encrypt(PlainText, SecretKey, Padding) -&gt; CipherText
</code></pre>

<ul class="definitions"><li><code>PlainText = binary()</code></li><li><code>SecretKey = <a href="#type-secret_key">secret_key()</a></code></li><li><code>Padding = rsa_pkcs1_padding | rsa_no_padding</code></li><li><code>CipherText = binary()</code></li></ul>

<a name="public_decrypt-2"></a>

### public_decrypt/2 ###

<pre><code>
public_decrypt(CipherText, PublicKey) -&gt; PlainText
</code></pre>

<ul class="definitions"><li><code>CipherText = binary()</code></li><li><code>PublicKey = <a href="#type-public_key">public_key()</a></code></li><li><code>PlainText = binary()</code></li></ul>

<a name="public_decrypt-3"></a>

### public_decrypt/3 ###

<pre><code>
public_decrypt(CipherText, PublicKey, Padding) -&gt; PlainText
</code></pre>

<ul class="definitions"><li><code>CipherText = binary()</code></li><li><code>PublicKey = <a href="#type-public_key">public_key()</a></code></li><li><code>Padding = rsa_pkcs1_padding | rsa_no_padding</code></li><li><code>PlainText = binary()</code></li></ul>

<a name="public_encrypt-2"></a>

### public_encrypt/2 ###

<pre><code>
public_encrypt(PlainText, PublicKey) -&gt; CipherText
</code></pre>

<ul class="definitions"><li><code>PlainText = binary()</code></li><li><code>PublicKey = <a href="#type-public_key">public_key()</a></code></li><li><code>CipherText = binary()</code></li></ul>

<a name="public_encrypt-3"></a>

### public_encrypt/3 ###

<pre><code>
public_encrypt(PlainText, PublicKey, Padding) -&gt; CipherText
</code></pre>

<ul class="definitions"><li><code>PlainText = binary()</code></li><li><code>PublicKey = <a href="#type-public_key">public_key()</a></code></li><li><code>Padding = rsa_pkcs1_padding | rsa_pkcs1_oaep_padding | rsa_no_padding</code></li><li><code>CipherText = binary()</code></li></ul>

<a name="secret_key_to_public_key-1"></a>

### secret_key_to_public_key/1 ###

<pre><code>
secret_key_to_public_key(SecretKey) -&gt; PublicKey
</code></pre>

<ul class="definitions"><li><code>SecretKey = <a href="#type-secret_key">secret_key()</a></code></li><li><code>PublicKey = <a href="#type-public_key">public_key()</a></code></li></ul>

<a name="sign-3"></a>

### sign/3 ###

<pre><code>
sign(DigestType, Message, SecretKey) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>DigestType = <a href="crypto.md#type-digest_type">crypto:digest_type()</a></code></li><li><code>Message = binary() | {digest, binary()}</code></li><li><code>SecretKey = <a href="#type-secret_key">secret_key()</a></code></li><li><code>Result = binary()</code></li></ul>

<a name="verify-4"></a>

### verify/4 ###

<pre><code>
verify(DigestType, Message, Signature, PublicKey) -&gt; boolean()
</code></pre>

<ul class="definitions"><li><code>DigestType = <a href="crypto.md#type-digest_type">crypto:digest_type()</a></code></li><li><code>Message = binary() | {digest, binary()}</code></li><li><code>Signature = binary()</code></li><li><code>PublicKey = <a href="#type-public_key">public_key()</a></code></li></ul>

