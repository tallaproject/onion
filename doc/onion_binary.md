

# Module onion_binary #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

Binary Utility API.

__Authors:__ Alexander Færøy ([`ahf@0x90.dk`](mailto:ahf@0x90.dk)).

<a name="description"></a>

## Description ##
This module contains various utility functions that are
found useful when working with binaries.
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#bit-2">bit/2</a></td><td>Get the bit value at a given position of a binary.</td></tr><tr><td valign="top"><a href="#bits-1">bits/1</a></td><td>Show the binary representation of a given binary.</td></tr><tr><td valign="top"><a href="#fingerprint-1">fingerprint/1</a></td><td>Get the fingerprint of a given binary.</td></tr><tr><td valign="top"><a href="#fingerprint-2">fingerprint/2</a></td><td>Apply the hash algorithm to the input and get the fingerprint.</td></tr><tr><td valign="top"><a href="#trim-2">trim/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="bit-2"></a>

### bit/2 ###

<pre><code>
bit(Subject, Position) -&gt; 0 | 1
</code></pre>

<ul class="definitions"><li><code>Subject = binary()</code></li><li><code>Position = non_neg_integer()</code></li></ul>

Get the bit value at a given position of a binary.

<a name="bits-1"></a>

### bits/1 ###

<pre><code>
bits(Subject) -&gt; [Bit]
</code></pre>

<ul class="definitions"><li><code>Subject = binary()</code></li><li><code>Bit = 0 | 1</code></li></ul>

Show the binary representation of a given binary.

<a name="fingerprint-1"></a>

### fingerprint/1 ###

<pre><code>
fingerprint(Data) -&gt; binary()
</code></pre>

<ul class="definitions"><li><code>Data = binary()</code></li></ul>

Get the fingerprint of a given binary.

<a name="fingerprint-2"></a>

### fingerprint/2 ###

<pre><code>
fingerprint(Hash, Data) -&gt; binary()
</code></pre>

<ul class="definitions"><li><code>Hash = <a href="crypto.md#type-hash_algorithm">crypto:hash_algorithm()</a></code></li><li><code>Data = binary()</code></li></ul>

Apply the hash algorithm to the input and get the fingerprint.

<a name="trim-2"></a>

### trim/2 ###

<pre><code>
trim(Subject, Pattern) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Subject = binary()</code></li><li><code>Pattern = binary() | [binary()]</code></li><li><code>Result = binary()</code></li></ul>

