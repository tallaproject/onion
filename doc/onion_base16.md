

# Module onion_base16 #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

Base16 API.

__Authors:__ Alexander Færøy ([`ahf@0x90.dk`](mailto:ahf@0x90.dk)).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#decode-1">decode/1</a></td><td></td></tr><tr><td valign="top"><a href="#encode-1">encode/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="decode-1"></a>

### decode/1 ###

<pre><code>
decode(Base16Data) -&gt; {ok, Data} | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>Base16Data = binary()</code></li><li><code>Data = binary()</code></li><li><code>Reason = term()</code></li></ul>

<a name="encode-1"></a>

### encode/1 ###

<pre><code>
encode(Data) -&gt; Base16Data
</code></pre>

<ul class="definitions"><li><code>Data = binary()</code></li><li><code>Base16Data = binary()</code></li></ul>

