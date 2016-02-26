

# Module onion_aes #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

AES API.

__Authors:__ Alexander Færøy ([`ahf@0x90.dk`](mailto:ahf@0x90.dk)).

<a name="types"></a>

## Data Types ##




### <a name="type-state">state()</a> ###


<pre><code>
state() = term()
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#decrypt-2">decrypt/2</a></td><td></td></tr><tr><td valign="top"><a href="#encrypt-2">encrypt/2</a></td><td></td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="decrypt-2"></a>

### decrypt/2 ###

<pre><code>
decrypt(State, CipherText) -&gt; {NewState, PlainText}
</code></pre>

<ul class="definitions"><li><code>State = <a href="#type-state">state()</a></code></li><li><code>CipherText = binary()</code></li><li><code>NewState = <a href="#type-state">state()</a></code></li><li><code>PlainText = binary()</code></li></ul>

<a name="encrypt-2"></a>

### encrypt/2 ###

<pre><code>
encrypt(State, PlainText) -&gt; {NewState, CipherText}
</code></pre>

<ul class="definitions"><li><code>State = <a href="#type-state">state()</a></code></li><li><code>PlainText = binary()</code></li><li><code>NewState = <a href="#type-state">state()</a></code></li><li><code>CipherText = binary()</code></li></ul>

<a name="init-1"></a>

### init/1 ###

<pre><code>
init(Key) -&gt; State
</code></pre>

<ul class="definitions"><li><code>Key = binary()</code></li><li><code>State = <a href="#type-state">state()</a></code></li></ul>

