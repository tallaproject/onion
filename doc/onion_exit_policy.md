

# Module onion_exit_policy #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

Exit Policy API.

__Authors:__ Alexander Færøy ([`ahf@0x90.dk`](mailto:ahf@0x90.dk)).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#default-0">default/0</a></td><td>Returns a sensible default policy: block everything.</td></tr><tr><td valign="top"><a href="#exit_allowed-3">exit_allowed/3</a></td><td>Check if a given Policy allows us to connect to the given Address and Port.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="default-0"></a>

### default/0 ###

<pre><code>
default() -&gt; [term()]
</code></pre>
<br />

Returns a sensible default policy: block everything.

<a name="exit_allowed-3"></a>

### exit_allowed/3 ###

<pre><code>
exit_allowed(Policy, Address, Port) -&gt; boolean()
</code></pre>

<ul class="definitions"><li><code>Policy = term()</code></li><li><code>Address = <a href="inet.md#type-ip_address">inet:ip_address()</a></code></li><li><code>Port = <a href="inet.md#type-port_number">inet:port_number()</a></code></li></ul>

Check if a given Policy allows us to connect to the given Address and Port.

