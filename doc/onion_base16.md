

# Module onion_base16 #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

Base16 API.

__Authors:__ Alexander Færøy ([`ahf@0x90.dk`](mailto:ahf@0x90.dk)).

<a name="types"></a>

## Data Types ##




### <a name="type-base16_encoded">base16_encoded()</a> ###


<pre><code>
base16_encoded() = binary()
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#decode-1">decode/1</a></td><td>Try to decode a given Base16 encoded binary.</td></tr><tr><td valign="top"><a href="#encode-1">encode/1</a></td><td>Encode a given binary in Base16 (Hex).</td></tr><tr><td valign="top"><a href="#valid-1">valid/1</a></td><td>Check if a given binary is valid Base16.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="decode-1"></a>

### decode/1 ###

<pre><code>
decode(Encoded) -&gt; {ok, Decoded} | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>Encoded = <a href="#type-base16_encoded">base16_encoded()</a></code></li><li><code>Decoded = binary()</code></li><li><code>Reason = term()</code></li></ul>

Try to decode a given Base16 encoded binary.

<a name="encode-1"></a>

### encode/1 ###

<pre><code>
encode(Data) -&gt; Encoded
</code></pre>

<ul class="definitions"><li><code>Data = binary()</code></li><li><code>Encoded = <a href="#type-base16_encoded">base16_encoded()</a></code></li></ul>

Encode a given binary in Base16 (Hex).

<a name="valid-1"></a>

### valid/1 ###

<pre><code>
valid(Data) -&gt; boolean()
</code></pre>

<ul class="definitions"><li><code>Data = binary() | string()</code></li></ul>

Check if a given binary is valid Base16.

