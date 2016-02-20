

# Module onion_server_descriptor #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

Server Descriptor API.

__Authors:__ Alexander Færøy ([`ahf@0x90.dk`](mailto:ahf@0x90.dk)).

<a name="description"></a>

## Description ##
This module contains functions for working with server descriptors. For
information about the server descriptor format see section 2.1.1 of Tor's
dir-spec.txt
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#encode-1">encode/1</a></td><td>Encode a given ServerDescriptor into an iolist().</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="encode-1"></a>

### encode/1 ###

<pre><code>
encode(ServerDescriptor) -&gt; iolist()
</code></pre>

<ul class="definitions"><li><code>ServerDescriptor = #{}</code></li></ul>

Encode a given ServerDescriptor into an iolist().

This function encodes a given ServerDescriptor into an iolist().

