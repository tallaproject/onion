

# Module onion_protocol #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

Protocol Utilities.

__Authors:__ Alexander Færøy ([`ahf@0x90.dk`](mailto:ahf@0x90.dk)).

<a name="description"></a>

## Description ##

<a name="types"></a>

## Data Types ##




### <a name="type-version">version()</a> ###


<pre><code>
version() = 3 | 4
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#shared_protocol-1">shared_protocol/1</a></td><td></td></tr><tr><td valign="top"><a href="#shared_protocol-2">shared_protocol/2</a></td><td></td></tr><tr><td valign="top"><a href="#supported-0">supported/0</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="shared_protocol-1"></a>

### shared_protocol/1 ###

<pre><code>
shared_protocol(Other) -&gt; {ok, <a href="#type-version">version()</a>} | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>Other = [<a href="#type-version">version()</a>]</code></li><li><code>Reason = term()</code></li></ul>

<a name="shared_protocol-2"></a>

### shared_protocol/2 ###

<pre><code>
shared_protocol(Other, Self) -&gt; {ok, <a href="#type-version">version()</a>} | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>Other = [<a href="#type-version">version()</a>]</code></li><li><code>Self = [<a href="#type-version">version()</a>]</code></li><li><code>Reason = term()</code></li></ul>

<a name="supported-0"></a>

### supported/0 ###

<pre><code>
supported() -&gt; [<a href="#type-version">version()</a>]
</code></pre>
<br />

