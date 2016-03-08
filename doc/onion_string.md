

# Module onion_string #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

String Utility API.

__Authors:__ Alexander Færøy ([`ahf@0x90.dk`](mailto:ahf@0x90.dk)).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#format-2">format/2</a></td><td></td></tr><tr><td valign="top"><a href="#valid-2">valid/2</a></td><td>Check a given string if it validates against a given alphabet.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="format-2"></a>

### format/2 ###

<pre><code>
format(Format, Data) -&gt; string()
</code></pre>

<ul class="definitions"><li><code>Format = string()</code></li><li><code>Data = [term()]</code></li></ul>

<a name="valid-2"></a>

### valid/2 ###

<pre><code>
valid(String, Alphabet) -&gt; boolean()
</code></pre>

<ul class="definitions"><li><code>String = string()</code></li><li><code>Alphabet = string()</code></li></ul>

Check a given string if it validates against a given alphabet.

This function checks a given string if all of its characters are a member
of a given alphabet.

