

# Module onion_base64 #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

Base64 wrapper API.

__Authors:__ Alexander Færøy ([`ahf@0x90.dk`](mailto:ahf@0x90.dk)).

<a name="description"></a>

## Description ##
This module contains a base64 API that allows us to encode
and decode binaries without the ordinary Base64 padding.

<a name="types"></a>

## Data Types ##




### <a name="type-base64_encoded">base64_encoded()</a> ###


<pre><code>
base64_encoded() = binary()
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#decode-1">decode/1</a></td><td></td></tr><tr><td valign="top"><a href="#encode-1">encode/1</a></td><td></td></tr><tr><td valign="top"><a href="#valid-1">valid/1</a></td><td>Check if a given binary is valid Base64.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="decode-1"></a>

### decode/1 ###

<pre><code>
decode(Encoded) -&gt; {ok, Decoded} | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>Encoded = <a href="#type-base64_encoded">base64_encoded()</a></code></li><li><code>Decoded = binary()</code></li><li><code>Reason = term()</code></li></ul>

<a name="encode-1"></a>

### encode/1 ###

<pre><code>
encode(Data) -&gt; Encoded
</code></pre>

<ul class="definitions"><li><code>Data = binary()</code></li><li><code>Encoded = <a href="#type-base64_encoded">base64_encoded()</a></code></li></ul>

<a name="valid-1"></a>

### valid/1 ###

<pre><code>
valid(Data) -&gt; boolean()
</code></pre>

<ul class="definitions"><li><code>Data = binary() | string()</code></li></ul>

Check if a given binary is valid Base64.

