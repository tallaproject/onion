

# Module onion_file #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

Filesystem API.

__Authors:__ Alexander Færøy ([`ahf@0x90.dk`](mailto:ahf@0x90.dk)).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#expand_tilde-1">expand_tilde/1</a></td><td>Expand "~" (tilde) in a path with the users home directory.</td></tr><tr><td valign="top"><a href="#homedir-0">homedir/0</a></td><td>Get the UNIX home directory.</td></tr><tr><td valign="top"><a href="#touch-2">touch/2</a></td><td>Create an empty file with a given UNIX mode.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="expand_tilde-1"></a>

### expand_tilde/1 ###

<pre><code>
expand_tilde(Path::<a href="file.md#type-filename">file:filename()</a>) -&gt; <a href="file.md#type-filename">file:filename()</a>
</code></pre>
<br />

Expand "~" (tilde) in a path with the users home directory.

<a name="homedir-0"></a>

### homedir/0 ###

<pre><code>
homedir() -&gt; <a href="file.md#type-filename">file:filename()</a>
</code></pre>
<br />

Get the UNIX home directory.

<a name="touch-2"></a>

### touch/2 ###

<pre><code>
touch(Filename, Mode) -&gt; ok | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>Filename = <a href="file.md#type-filename">file:filename()</a></code></li><li><code>Mode = integer()</code></li><li><code>Reason = term()</code></li></ul>

Create an empty file with a given UNIX mode.

