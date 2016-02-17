

# Module onion_dh #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

Diffie-Hellman Utilities.

__Authors:__ Alexander Færøy ([`ahf@0x90.dk`](mailto:ahf@0x90.dk)).

<a name="types"></a>

## Data Types ##




### <a name="type-keypair">keypair()</a> ###


<pre><code>
keypair() = #{secret =&gt; <a href="crypto.md#type-dh_private">crypto:dh_private()</a>, public =&gt; <a href="crypto.md#type-dh_public">crypto:dh_public()</a>}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#keypair-0">keypair/0</a></td><td></td></tr><tr><td valign="top"><a href="#params-0">params/0</a></td><td></td></tr><tr><td valign="top"><a href="#shared_secret-2">shared_secret/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="keypair-0"></a>

### keypair/0 ###

<pre><code>
keypair() -&gt; <a href="#type-keypair">keypair()</a>
</code></pre>
<br />

<a name="params-0"></a>

### params/0 ###

<pre><code>
params() -&gt; [non_neg_integer()]
</code></pre>
<br />

<a name="shared_secret-2"></a>

### shared_secret/2 ###

<pre><code>
shared_secret(SecretKey, PublicKey) -&gt; SharedSecret
</code></pre>

<ul class="definitions"><li><code>SecretKey = <a href="crypto.md#type-dh_private">crypto:dh_private()</a></code></li><li><code>PublicKey = <a href="crypto.md#type-dh_public">crypto:dh_public()</a></code></li><li><code>SharedSecret = binary()</code></li></ul>

