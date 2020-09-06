

# Module onion_circuit #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

Circuit Utility Functions.

__Authors:__ Alexander Færøy ([`ahf@0x90.dk`](mailto:ahf@0x90.dk)).

<a name="types"></a>

## Data Types ##




### <a name="type-id">id()</a> ###


<pre><code>
id() = 0..4294967295
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#id-2">id/2</a></td><td>Create a new randomly chosen Circuit ID.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="id-2"></a>

### id/2 ###

<pre><code>
id(ProtocolVersion, HighBit) -&gt; {ok, <a href="#type-id">id()</a>} | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>ProtocolVersion = <a href="onion_protocol.md#type-version">onion_protocol:version()</a></code></li><li><code>HighBit = boolean()</code></li><li><code>Reason = term()</code></li></ul>

Create a new randomly chosen Circuit ID

