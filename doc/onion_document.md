

# Module onion_document #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

Document API.

__Authors:__ Alexander Færøy ([`ahf@0x90.dk`](mailto:ahf@0x90.dk)).

<a name="description"></a>

## Description ##
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#decode-1">decode/1</a></td><td></td></tr><tr><td valign="top"><a href="#ed25519_sign-2">ed25519_sign/2</a></td><td></td></tr><tr><td valign="top"><a href="#encode-1">encode/1</a></td><td>Encode a given Document into an iolist().</td></tr><tr><td valign="top"><a href="#get_item-2">get_item/2</a></td><td></td></tr><tr><td valign="top"><a href="#rsa_sign-2">rsa_sign/2</a></td><td></td></tr><tr><td valign="top"><a href="#split-2">split/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="decode-1"></a>

### decode/1 ###

<pre><code>
decode(Data) -&gt; {ok, [Item]} | {error, term()}
</code></pre>

<ul class="definitions"><li><code>Data = binary()</code></li><li><code>Item = {Keyword, Arguments} | {Keyword, Arguments, Object}</code></li><li><code>Keyword = binary()</code></li><li><code>Arguments = [binary()]</code></li><li><code>Object = [binary()]</code></li></ul>

<a name="ed25519_sign-2"></a>

### ed25519_sign/2 ###

`ed25519_sign(Document, SecretKey) -> any()`

<a name="encode-1"></a>

### encode/1 ###

<pre><code>
encode(Document) -&gt; Data
</code></pre>

<ul class="definitions"><li><code>Document = [Item]</code></li><li><code>Item = {Keyword, Arguments, Objects}</code></li><li><code>Keyword = string() | atom() | binary()</code></li><li><code>Arguments = [binary()]</code></li><li><code>Objects = [Object]</code></li><li><code>Object = term()</code></li><li><code>Data = binary()</code></li></ul>

Encode a given Document into an iolist().

This function encodes a given Document into an iolist().

<a name="get_item-2"></a>

### get_item/2 ###

<pre><code>
get_item(Keyword, Document) -&gt; Item | not_found
</code></pre>

<ul class="definitions"><li><code>Keyword = binary()</code></li><li><code>Arguments = [binary()]</code></li><li><code>Object = [binary()]</code></li><li><code>Item = {Keyword, Arguments} | {Keyword, Arguments, Object}</code></li><li><code>Document = [Item]</code></li></ul>

<a name="rsa_sign-2"></a>

### rsa_sign/2 ###

`rsa_sign(Document, SecretKey) -> any()`

<a name="split-2"></a>

### split/2 ###

<pre><code>
split(Document, Keyword) -&gt; [Document]
</code></pre>

<ul class="definitions"><li><code>Keyword = binary()</code></li><li><code>Arguments = [binary()]</code></li><li><code>Object = [binary()]</code></li><li><code>Item = {Keyword, Arguments} | {Keyword, Arguments, Object}</code></li><li><code>Document = [Item]</code></li></ul>

