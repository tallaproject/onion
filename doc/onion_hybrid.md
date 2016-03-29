

# Module onion_hybrid #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

Tor Hybrid Encryption using RSA and AES.

__Authors:__ Alexander Færøy ([`ahf@0x90.dk`](mailto:ahf@0x90.dk)).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#decrypt-2">decrypt/2</a></td><td>Decrypt a given message.</td></tr><tr><td valign="top"><a href="#encrypt-2">encrypt/2</a></td><td>Encrypt a given message.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="decrypt-2"></a>

### decrypt/2 ###

<pre><code>
decrypt(CipherText, SecretKey) -&gt; PlainText
</code></pre>

<ul class="definitions"><li><code>CipherText = binary()</code></li><li><code>SecretKey = <a href="onion_rsa.md#type-secret_key">onion_rsa:secret_key()</a></code></li><li><code>PlainText = binary()</code></li></ul>

Decrypt a given message.

<a name="encrypt-2"></a>

### encrypt/2 ###

<pre><code>
encrypt(PlainText, PublicKey) -&gt; CipherText
</code></pre>

<ul class="definitions"><li><code>PlainText = binary()</code></li><li><code>PublicKey = <a href="onion_rsa.md#type-public_key">onion_rsa:public_key()</a></code></li><li><code>CipherText = binary()</code></li></ul>

Encrypt a given message.

