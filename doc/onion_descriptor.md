

# Module onion_descriptor #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

Directory Descriptor API.

__Authors:__ Alexander Færøy ([`ahf@0x90.dk`](mailto:ahf@0x90.dk)).

<a name="description"></a>

## Description ##

<a name="types"></a>

## Data Types ##




### <a name="type-address">address()</a> ###


<pre><code>
address() = <a href="inet.md#type-ip_address">inet:ip_address()</a>
</code></pre>




### <a name="type-bandwidth_average">bandwidth_average()</a> ###


<pre><code>
bandwidth_average() = <a href="#type-bytes_per_second">bytes_per_second()</a>
</code></pre>




### <a name="type-bandwidth_burst">bandwidth_burst()</a> ###


<pre><code>
bandwidth_burst() = <a href="#type-bytes_per_second">bytes_per_second()</a>
</code></pre>




### <a name="type-bandwidth_observed">bandwidth_observed()</a> ###


<pre><code>
bandwidth_observed() = <a href="#type-bytes_per_second">bytes_per_second()</a>
</code></pre>




### <a name="type-bytes_per_second">bytes_per_second()</a> ###


<pre><code>
bytes_per_second() = non_neg_integer()
</code></pre>




### <a name="type-contact">contact()</a> ###


<pre><code>
contact() = string()
</code></pre>




### <a name="type-dir_port">dir_port()</a> ###


<pre><code>
dir_port() = <a href="inet.md#type-port_number">inet:port_number()</a>
</code></pre>




### <a name="type-exit_policy">exit_policy()</a> ###


<pre><code>
exit_policy() = [<a href="#type-exit_policy_entry">exit_policy_entry()</a>]
</code></pre>




### <a name="type-exit_policy_entry">exit_policy_entry()</a> ###


<pre><code>
exit_policy_entry() = {allow, string()} | {reject, string()}
</code></pre>




### <a name="type-nickname">nickname()</a> ###


<pre><code>
nickname() = string()
</code></pre>




### <a name="type-or_port">or_port()</a> ###


<pre><code>
or_port() = <a href="inet.md#type-port_number">inet:port_number()</a>
</code></pre>




### <a name="type-platform">platform()</a> ###


<pre><code>
platform() = string()
</code></pre>




### <a name="type-seconds">seconds()</a> ###


<pre><code>
seconds() = non_neg_integer()
</code></pre>




### <a name="type-server_descriptor">server_descriptor()</a> ###


<pre><code>
server_descriptor() = [<a href="#type-server_descriptor_entry">server_descriptor_entry()</a>]
</code></pre>




### <a name="type-server_descriptor_entry">server_descriptor_entry()</a> ###


<pre><code>
server_descriptor_entry() = {router, <a href="#type-nickname">nickname()</a>, <a href="#type-address">address()</a>, <a href="#type-or_port">or_port()</a>, <a href="#type-dir_port">dir_port()</a>} | {platform, <a href="#type-platform">platform()</a>} | {contact, <a href="#type-contact">contact()</a>} | {published, <a href="calendar.md#type-datetime">calendar:datetime()</a>} | {fingerprint, <a href="onion_rsa.md#type-public_key">onion_rsa:public_key()</a>} | {uptime, <a href="#type-seconds">seconds()</a>} | {bandwidth, <a href="#type-bandwidth_average">bandwidth_average()</a>, <a href="#type-bandwidth_burst">bandwidth_burst()</a>, <a href="#type-bandwidth_observed">bandwidth_observed()</a>} | {onion_key, <a href="onion_rsa.md#type-public_key">onion_rsa:public_key()</a>} | {signing_key, <a href="onion_rsa.md#type-public_key">onion_rsa:public_key()</a>} | {ntor_onion_key, <a href="onion_curve25519.md#type-public_key">onion_curve25519:public_key()</a>} | {exit_policy, <a href="#type-exit_policy">exit_policy()</a>}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#render-2">render/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="render-2"></a>

### render/2 ###

<pre><code>
render(ServerDescriptor, SecretKey) -&gt; {ok, binary()} | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>ServerDescriptor = <a href="#type-server_descriptor">server_descriptor()</a></code></li><li><code>SecretKey = <a href="onion_rsa.md#type-secret_key">onion_rsa:secret_key()</a></code></li><li><code>Reason = term()</code></li></ul>

