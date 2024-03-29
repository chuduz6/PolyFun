


<!DOCTYPE html>
<html>
  <head>
    <meta charset='utf-8'>
    <meta http-equiv="X-UA-Compatible" content="chrome=1">
        <title>math_util.lisp at master from pedrosans/computational_mathematics - GitHub</title>
    <link rel="search" type="application/opensearchdescription+xml" href="/opensearch.xml" title="GitHub" />
    <link rel="fluid-icon" href="https://github.com/fluidicon.png" title="GitHub" />

    
    

    <meta content="authenticity_token" name="csrf-param" />
<meta content="75f5792e0d1578db9eb3edaff48052cb33967218" name="csrf-token" />

    <link href="https://a248.e.akamai.net/assets.github.com/stylesheets/bundle_github.css?5d20147077c599098f68712cc3fef34df0cb3bcd" media="screen" rel="stylesheet" type="text/css" />
    

    <script src="https://a248.e.akamai.net/assets.github.com/javascripts/bundle_jquery.js?1cda878e9113acce74087d5ff1a5e1c49fb94eeb" type="text/javascript"></script>
    <script src="https://a248.e.akamai.net/assets.github.com/javascripts/bundle_github.js?88c2b5bccd6a2a3b368f3ed58fe6fcf0582e5ca6" type="text/javascript"></script>
    

      <link rel='permalink' href='/pedrosans/computational_mathematics/blob/e471c1bf0c0c8db7d6b63675658a161389317a47/math_util.lisp'>
    

    <meta name="description" content="computational_mathematics - Set of algorithms in LISP to solve common computational mathematics problems" />
  <link href="https://github.com/pedrosans/computational_mathematics/commits/master.atom" rel="alternate" title="Recent Commits to computational_mathematics:master" type="application/atom+xml" />

  </head>


  <body class="logged_out page-blob windows env-production ">
    


    

    <div id="main">
      <div id="header" class="true">
          <a class="logo" href="https://github.com">
            <img alt="github" class="default svg" height="45" src="https://a248.e.akamai.net/assets.github.com/images/modules/header/logov6.svg" />
            <img alt="github" class="default png" height="45" src="https://a248.e.akamai.net/assets.github.com/images/modules/header/logov6.png" />
            <!--[if (gt IE 8)|!(IE)]><!-->
            <img alt="github" class="hover svg" height="45" src="https://a248.e.akamai.net/assets.github.com/images/modules/header/logov6-hover.svg" />
            <img alt="github" class="hover png" height="45" src="https://a248.e.akamai.net/assets.github.com/images/modules/header/logov6-hover.png" />
            <!--<![endif]-->
          </a>

        <div class="topsearch">
    <!--
      make sure to use fully qualified URLs here since this nav
      is used on error pages on other domains
    -->
    <ul class="nav logged_out">
        <li class="pricing"><a href="https://github.com/plans">Signup and Pricing</a></li>
        <li class="explore"><a href="https://github.com/explore">Explore GitHub</a></li>
      <li class="features"><a href="https://github.com/features">Features</a></li>
        <li class="blog"><a href="https://github.com/blog">Blog</a></li>
      <li class="login"><a href="https://github.com/login?return_to=%2Fpedrosans%2Fcomputational_mathematics%2Fblob%2Fmaster%2Fmath_util.lisp">Login</a></li>
    </ul>
</div>

      </div>

      
            <div class="site">
      <div class="pagehead repohead vis-public   instapaper_ignore readability-menu">


      <div class="title-actions-bar">
        <h1>
          <a href="/pedrosans">pedrosans</a> /
          <strong><a href="/pedrosans/computational_mathematics" class="js-current-repository">computational_mathematics</a></strong>
        </h1>
        



            <ul class="pagehead-actions">

        <li>
            <a href="/pedrosans/computational_mathematics/toggle_watch" class="minibutton btn-watch watch-button" data-method="post"><span><span class="icon"></span>Watch</span></a>
        </li>
            <li><a href="/pedrosans/computational_mathematics/fork" class="minibutton btn-fork fork-button" data-method="post"><span><span class="icon"></span>Fork</span></a></li>

      <li class="repostats">
        <ul class="repo-stats">
          <li class="watchers ">
            <a href="/pedrosans/computational_mathematics/watchers" title="Watchers" class="tooltipped downwards">
              3
            </a>
          </li>
          <li class="forks">
            <a href="/pedrosans/computational_mathematics/network" title="Forks" class="tooltipped downwards">
              2
            </a>
          </li>
        </ul>
      </li>
    </ul>

      </div>

        

  <ul class="tabs">
    <li><a href="/pedrosans/computational_mathematics" class="selected" highlight="repo_sourcerepo_downloadsrepo_commitsrepo_tagsrepo_branches">Code</a></li>
    <li><a href="/pedrosans/computational_mathematics/network" highlight="repo_networkrepo_fork_queue">Network</a>
    <li><a href="/pedrosans/computational_mathematics/pulls" highlight="repo_pulls">Pull Requests <span class='counter'>1</span></a></li>

      <li><a href="/pedrosans/computational_mathematics/issues" highlight="repo_issues">Issues <span class='counter'>1</span></a></li>


    <li><a href="/pedrosans/computational_mathematics/graphs" highlight="repo_graphsrepo_contributors">Stats &amp; Graphs</a></li>

  </ul>

  
<div class="frame frame-center tree-finder" style="display:none"
      data-tree-list-url="/pedrosans/computational_mathematics/tree-list/e471c1bf0c0c8db7d6b63675658a161389317a47"
      data-blob-url-prefix="/pedrosans/computational_mathematics/blob/e471c1bf0c0c8db7d6b63675658a161389317a47"
    >

  <div class="breadcrumb">
    <b><a href="/pedrosans/computational_mathematics">computational_mathematics</a></b> /
    <input class="tree-finder-input" type="text" name="query" autocomplete="off" spellcheck="false">
  </div>

    <div class="octotip">
      <p>
        <a href="/pedrosans/computational_mathematics/dismiss-tree-finder-help" class="dismiss js-dismiss-tree-list-help" title="Hide this notice forever">Dismiss</a>
        <strong>Octotip:</strong> You've activated the <em>file finder</em>
        by pressing <span class="kbd">t</span> Start typing to filter the
        file list. Use <span class="kbd badmono">↑</span> and
        <span class="kbd badmono">↓</span> to navigate,
        <span class="kbd">enter</span> to view files.
      </p>
    </div>

  <table class="tree-browser" cellpadding="0" cellspacing="0">
    <tr class="js-header"><th>&nbsp;</th><th>name</th></tr>
    <tr class="js-no-results no-results" style="display: none">
      <th colspan="2">No matching files</th>
    </tr>
    <tbody class="js-results-list">
    </tbody>
  </table>
</div>

<div id="jump-to-line" style="display:none">
  <h2>Jump to Line</h2>
  <form>
    <input class="textfield" type="text">
    <div class="full-button">
      <button type="submit" class="classy">
        <span>Go</span>
      </button>
    </div>
  </form>
</div>


<div class="subnav-bar">

  <ul class="actions">
    
      <li class="switcher">

        <div class="context-menu-container js-menu-container">
          <span class="text">Current branch:</span>
          <a href="#"
             class="minibutton bigger switcher context-menu-button js-menu-target js-commitish-button btn-branch repo-tree"
             data-master-branch="master"
             data-ref="master">
            <span><span class="icon"></span>master</span>
          </a>

          <div class="context-pane commitish-context js-menu-content">
            <a href="javascript:;" class="close js-menu-close"></a>
            <div class="title">Switch Branches/Tags</div>
            <div class="body pane-selector commitish-selector js-filterable-commitishes">
              <div class="filterbar">
                <div class="placeholder-field js-placeholder-field">
                  <label class="placeholder" for="context-commitish-filter-field" data-placeholder-mode="sticky">Filter branches/tags</label>
                  <input type="text" id="context-commitish-filter-field" class="commitish-filter" />
                </div>

                <ul class="tabs">
                  <li><a href="#" data-filter="branches" class="selected">Branches</a></li>
                  <li><a href="#" data-filter="tags">Tags</a></li>
                </ul>
              </div>

                <div class="commitish-item branch-commitish selector-item">
                  <h4>
                      <a href="/pedrosans/computational_mathematics/blob/master/math_util.lisp" data-name="master">master</a>
                  </h4>
                </div>


              <div class="no-results" style="display:none">Nothing to show</div>
            </div>
          </div><!-- /.commitish-context-context -->
        </div>

      </li>
  </ul>

  <ul class="subnav">
    <li><a href="/pedrosans/computational_mathematics" class="selected" highlight="repo_source">Files</a></li>
    <li><a href="/pedrosans/computational_mathematics/commits/master" highlight="repo_commits">Commits</a></li>
    <li><a href="/pedrosans/computational_mathematics/branches" class="" highlight="repo_branches">Branches <span class="counter">1</span></a></li>
    <li><a href="/pedrosans/computational_mathematics/tags" class="blank" highlight="repo_tags">Tags <span class="counter">0</span></a></li>
    <li><a href="/pedrosans/computational_mathematics/downloads" class="blank" highlight="repo_downloads">Downloads <span class="counter">0</span></a></li>
  </ul>

</div>

  
  
  


        

      </div><!-- /.pagehead -->

      




  
  <p class="last-commit">Latest commit to the <strong>master</strong> branch</p>

<div class="commit commit-tease js-details-container">
  <p class="commit-title ">
      <a href="/pedrosans/computational_mathematics"><a href="/pedrosans/computational_mathematics/commit/e471c1bf0c0c8db7d6b63675658a161389317a47" class="message">gauss-seidel development</a></a>
      
  </p>
  <div class="commit-meta">
    <a href="/pedrosans/computational_mathematics/commit/e471c1bf0c0c8db7d6b63675658a161389317a47" class="sha-block">commit <span class="sha">e471c1bf0c</span></a>

    <div class="authorship">
      <img class="gravatar" height="20" src="https://secure.gravatar.com/avatar/9e5c3983091234d0a6a631ee8485ec7a?s=140&amp;d=https://a248.e.akamai.net/assets.github.com%2Fimages%2Fgravatars%2Fgravatar-140.png" width="20" />
      <span class="author-name"><a href="/pedrosans">pedrosans</a></span>
      authored <time class="js-relative-date" datetime="2011-05-24T13:51:15-07:00" title="2011-05-24 13:51:15">May 24, 2011</time>

    </div>
  </div>
</div>


  <div id="slider">

    <div class="breadcrumb" data-path="math_util.lisp/">
      <b><a href="/pedrosans/computational_mathematics/tree/e471c1bf0c0c8db7d6b63675658a161389317a47" class="js-rewrite-sha">computational_mathematics</a></b> / math_util.lisp       <span style="display:none" id="clippy_202" class="clippy-text">math_util.lisp</span>
      
      <object classid="clsid:d27cdb6e-ae6d-11cf-96b8-444553540000"
              width="110"
              height="14"
              class="clippy"
              id="clippy" >
      <param name="movie" value="https://a248.e.akamai.net/assets.github.com/flash/clippy.swf?v5"/>
      <param name="allowScriptAccess" value="always" />
      <param name="quality" value="high" />
      <param name="scale" value="noscale" />
      <param NAME="FlashVars" value="id=clippy_202&amp;copied=copied!&amp;copyto=copy to clipboard">
      <param name="bgcolor" value="#FFFFFF">
      <param name="wmode" value="opaque">
      <embed src="https://a248.e.akamai.net/assets.github.com/flash/clippy.swf?v5"
             width="110"
             height="14"
             name="clippy"
             quality="high"
             allowScriptAccess="always"
             type="application/x-shockwave-flash"
             pluginspage="http://www.macromedia.com/go/getflashplayer"
             FlashVars="id=clippy_202&amp;copied=copied!&amp;copyto=copy to clipboard"
             bgcolor="#FFFFFF"
             wmode="opaque"
      />
      </object>
      

    </div>

    <div class="frames">
      <div class="frame frame-center" data-path="math_util.lisp/" data-permalink-url="/pedrosans/computational_mathematics/blob/e471c1bf0c0c8db7d6b63675658a161389317a47/math_util.lisp" data-title="math_util.lisp at master from pedrosans/computational_mathematics - GitHub" data-type="blob">
          <ul class="big-actions">
            <li><a class="file-edit-link minibutton js-rewrite-sha" href="/pedrosans/computational_mathematics/edit/e471c1bf0c0c8db7d6b63675658a161389317a47/math_util.lisp" data-method="post"><span>Edit this file</span></a></li>
          </ul>

        <div id="files">
          <div class="file">
            <div class="meta">
              <div class="info">
                <span class="icon"><img alt="Txt" height="16" src="https://a248.e.akamai.net/assets.github.com/images/icons/txt.png" width="16" /></span>
                <span class="mode" title="File Mode">100644</span>
                  <span>41 lines (40 sloc)</span>
                <span>1.953 kb</span>
              </div>
              <ul class="actions">
                <li><a href="/pedrosans/computational_mathematics/raw/master/math_util.lisp" id="raw-url">raw</a></li>
                  <li><a href="/pedrosans/computational_mathematics/blame/master/math_util.lisp">blame</a></li>
                <li><a href="/pedrosans/computational_mathematics/commits/master/math_util.lisp">history</a></li>
              </ul>
            </div>
              <div class="data type-common-lisp">
      <table cellpadding="0" cellspacing="0" class="lines">
        <tr>
          <td>
            <pre class="line_numbers"><span id="L1" rel="#L1">1</span>
<span id="L2" rel="#L2">2</span>
<span id="L3" rel="#L3">3</span>
<span id="L4" rel="#L4">4</span>
<span id="L5" rel="#L5">5</span>
<span id="L6" rel="#L6">6</span>
<span id="L7" rel="#L7">7</span>
<span id="L8" rel="#L8">8</span>
<span id="L9" rel="#L9">9</span>
<span id="L10" rel="#L10">10</span>
<span id="L11" rel="#L11">11</span>
<span id="L12" rel="#L12">12</span>
<span id="L13" rel="#L13">13</span>
<span id="L14" rel="#L14">14</span>
<span id="L15" rel="#L15">15</span>
<span id="L16" rel="#L16">16</span>
<span id="L17" rel="#L17">17</span>
<span id="L18" rel="#L18">18</span>
<span id="L19" rel="#L19">19</span>
<span id="L20" rel="#L20">20</span>
<span id="L21" rel="#L21">21</span>
<span id="L22" rel="#L22">22</span>
<span id="L23" rel="#L23">23</span>
<span id="L24" rel="#L24">24</span>
<span id="L25" rel="#L25">25</span>
<span id="L26" rel="#L26">26</span>
<span id="L27" rel="#L27">27</span>
<span id="L28" rel="#L28">28</span>
<span id="L29" rel="#L29">29</span>
<span id="L30" rel="#L30">30</span>
<span id="L31" rel="#L31">31</span>
<span id="L32" rel="#L32">32</span>
<span id="L33" rel="#L33">33</span>
<span id="L34" rel="#L34">34</span>
<span id="L35" rel="#L35">35</span>
<span id="L36" rel="#L36">36</span>
<span id="L37" rel="#L37">37</span>
<span id="L38" rel="#L38">38</span>
<span id="L39" rel="#L39">39</span>
<span id="L40" rel="#L40">40</span>
<span id="L41" rel="#L41">41</span>
</pre>
          </td>
          <td width="100%">
                <div class="highlight"><pre><div class='line' id='LC1'><span class="c1">;----------------------------------------------------------------------------------------------------------------------------------</span></div><div class='line' id='LC2'><span class="c1">;;from http://common-lisp.net/project/elephant/darcs/stdutils/matrix.lisp</span></div><div class='line' id='LC3'><span class="p">(</span><span class="nb">defun</span> <span class="nv">invert-matrix</span> <span class="p">(</span><span class="nv">matrix</span> <span class="k">&amp;optional</span> <span class="p">(</span><span class="nv">destructive</span> <span class="no">T</span><span class="p">))</span></div><div class='line' id='LC4'>&nbsp;<span class="s">&quot;Find the inverse of a matrix.  By default this operation is</span></div><div class='line' id='LC5'><span class="s"> destructive.  If you want to preserve the original matrix, call this</span></div><div class='line' id='LC6'><span class="s"> function with an argument of NIL to destructive.&quot;</span></div><div class='line' id='LC7'>&nbsp;<span class="p">(</span><span class="k">let</span> <span class="p">((</span><span class="nv">result</span> <span class="p">(</span><span class="k">if</span> <span class="nv">destructive</span> <span class="nv">matrix</span> <span class="p">(</span><span class="nv">copy-matrix</span> <span class="nv">matrix</span><span class="p">)))</span></div><div class='line' id='LC8'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">size</span> <span class="p">(</span><span class="nb">array-dimension</span> <span class="nv">matrix</span> <span class="mi">0</span><span class="p">))</span></div><div class='line' id='LC9'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">temp</span> <span class="mi">0</span><span class="p">))</span></div><div class='line' id='LC10'>&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">dotimes</span> <span class="p">(</span><span class="nv">i</span> <span class="nv">size</span> <span class="nv">result</span><span class="p">)</span></div><div class='line' id='LC11'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">setf</span> <span class="nv">temp</span> <span class="p">(</span><span class="nb">aref</span> <span class="nv">result</span> <span class="nv">i</span> <span class="nv">i</span><span class="p">))</span></div><div class='line' id='LC12'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">dotimes</span> <span class="p">(</span><span class="nv">j</span> <span class="nv">size</span><span class="p">)</span></div><div class='line' id='LC13'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">setf</span> <span class="p">(</span><span class="nb">aref</span> <span class="nv">result</span> <span class="nv">i</span> <span class="nv">j</span><span class="p">)</span></div><div class='line' id='LC14'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="k">if</span> <span class="p">(</span><span class="nb">=</span> <span class="nv">i</span> <span class="nv">j</span><span class="p">)</span></div><div class='line' id='LC15'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="k">if</span> <span class="p">(</span><span class="nb">=</span><span class="p">(</span><span class="nb">aref</span> <span class="nv">result</span> <span class="nv">i</span> <span class="nv">j</span><span class="p">)</span> <span class="mi">0</span><span class="p">)</span> <span class="mi">0</span> <span class="p">(</span><span class="nb">/</span> <span class="p">(</span><span class="nb">aref</span> <span class="nv">result</span> <span class="nv">i</span> <span class="nv">j</span><span class="p">))</span> <span class="p">)</span></div><div class='line' id='LC16'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="k">if</span> <span class="p">(</span><span class="nb">=</span> <span class="nv">temp</span> <span class="mi">0</span><span class="p">)</span> <span class="mi">0</span> <span class="p">(</span><span class="nb">/</span> <span class="p">(</span><span class="nb">aref</span> <span class="nv">result</span> <span class="nv">i</span> <span class="nv">j</span><span class="p">)</span> <span class="nv">temp</span><span class="p">))</span>               <span class="p">)))</span></div><div class='line' id='LC17'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">dotimes</span> <span class="p">(</span><span class="nv">j</span> <span class="nv">size</span><span class="p">)</span></div><div class='line' id='LC18'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">unless</span> <span class="p">(</span><span class="nb">=</span> <span class="nv">i</span> <span class="nv">j</span><span class="p">)</span></div><div class='line' id='LC19'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">setf</span> <span class="nv">temp</span> <span class="p">(</span><span class="nb">aref</span> <span class="nv">result</span> <span class="nv">j</span> <span class="nv">i</span><span class="p">)</span></div><div class='line' id='LC20'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">aref</span> <span class="nv">result</span> <span class="nv">j</span> <span class="nv">i</span><span class="p">)</span> <span class="mi">0</span><span class="p">)</span></div><div class='line' id='LC21'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">dotimes</span> <span class="p">(</span><span class="nv">k</span> <span class="nv">size</span><span class="p">)</span></div><div class='line' id='LC22'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">setf</span> <span class="p">(</span><span class="nb">aref</span> <span class="nv">result</span> <span class="nv">j</span> <span class="nv">k</span><span class="p">)</span></div><div class='line' id='LC23'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">-</span> <span class="p">(</span><span class="nb">aref</span> <span class="nv">result</span> <span class="nv">j</span> <span class="nv">k</span><span class="p">)</span></div><div class='line' id='LC24'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">*</span> <span class="nv">temp</span> <span class="p">(</span><span class="nb">aref</span> <span class="nv">result</span> <span class="nv">i</span> <span class="nv">k</span><span class="p">))))))))))</span></div><div class='line' id='LC25'><span class="c1">;----------------------------------------------------------------------------------------------------------------------------------</span></div><div class='line' id='LC26'><span class="c1">;;from http://rosettacode.org/wiki/Matrix_multiplication#Common_Lisp</span></div><div class='line' id='LC27'><span class="p">(</span><span class="nb">defun</span> <span class="nv">matrix-multiply</span> <span class="p">(</span><span class="nv">a</span> <span class="nv">b</span><span class="p">)</span></div><div class='line' id='LC28'>&nbsp;<span class="p">(</span><span class="k">flet</span> <span class="p">((</span><span class="nv">col</span> <span class="p">(</span><span class="nv">mat</span> <span class="nv">i</span><span class="p">)</span> <span class="p">(</span><span class="nb">mapcar</span> <span class="nf">#&#39;</span><span class="p">(</span><span class="k">lambda</span> <span class="p">(</span><span class="nv">row</span><span class="p">)</span> <span class="p">(</span><span class="nb">elt</span> <span class="nv">row</span> <span class="nv">i</span><span class="p">))</span> <span class="nv">mat</span><span class="p">))</span></div><div class='line' id='LC29'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nv">row</span> <span class="p">(</span><span class="nv">mat</span> <span class="nv">i</span><span class="p">)</span> <span class="p">(</span><span class="nb">elt</span> <span class="nv">mat</span> <span class="nv">i</span><span class="p">)))</span></div><div class='line' id='LC30'>&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">loop</span> <span class="nv">for</span> <span class="nv">row</span> <span class="nv">from</span> <span class="mi">0</span> <span class="nv">below</span> <span class="p">(</span><span class="nb">length</span> <span class="nv">a</span><span class="p">)</span></div><div class='line' id='LC31'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nv">collect</span> <span class="p">(</span><span class="nb">loop</span> <span class="nv">for</span> <span class="nv">col</span> <span class="nv">from</span> <span class="mi">0</span> <span class="nv">below</span> <span class="p">(</span><span class="nb">length</span> <span class="p">(</span><span class="nv">row</span> <span class="nv">b</span> <span class="mi">0</span><span class="p">))</span></div><div class='line' id='LC32'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nv">collect</span> <span class="p">(</span><span class="nb">apply</span> <span class="nf">#&#39;</span><span class="nb">+</span> <span class="p">(</span><span class="nb">mapcar</span> <span class="nf">#&#39;</span><span class="nb">*</span> <span class="p">(</span><span class="nv">row</span> <span class="nv">a</span> <span class="nv">row</span><span class="p">)</span></div><div class='line' id='LC33'><span class="p">(</span><span class="nv">col</span> <span class="nv">b</span> <span class="nv">col</span><span class="p">)))))))</span></div><div class='line' id='LC34'><span class="c1">;-----------------------------------------------------------------------------------------------------------------------------</span></div><div class='line' id='LC35'><span class="c1">;;from http://aleteia.wikidot.com/lisp:derivada</span></div><div class='line' id='LC36'><span class="p">(</span><span class="nb">defun</span> <span class="nv">deriv</span> <span class="p">(</span><span class="nv">fn</span><span class="p">)</span></div><div class='line' id='LC37'>&nbsp;<span class="p">(</span><span class="k">let</span> <span class="p">((</span><span class="nv">dx</span> <span class="m">1/1000000000</span><span class="p">))</span></div><div class='line' id='LC38'>&nbsp;&nbsp;&nbsp;<span class="nf">#&#39;</span><span class="p">(</span><span class="k">lambda</span> <span class="p">(</span><span class="nv">x</span><span class="p">)</span></div><div class='line' id='LC39'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="p">(</span><span class="nb">/</span> <span class="p">(</span><span class="nb">-</span> <span class="p">(</span><span class="nb">funcall</span> <span class="nv">fn</span> <span class="p">(</span><span class="nb">+</span> <span class="nv">x</span> <span class="nv">dx</span><span class="p">))</span> <span class="p">(</span><span class="nb">funcall</span> <span class="nv">fn</span> <span class="nv">x</span><span class="p">))</span></div><div class='line' id='LC40'>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="nv">dx</span><span class="p">))))</span></div><div class='line' id='LC41'><br/></div></pre></div>
          </td>
        </tr>
      </table>
  </div>

          </div>
        </div>
      </div>
    </div>

  </div>

<div class="frame frame-loading" style="display:none;" data-tree-list-url="/pedrosans/computational_mathematics/tree-list/e471c1bf0c0c8db7d6b63675658a161389317a47" data-blob-url-prefix="/pedrosans/computational_mathematics/blob/e471c1bf0c0c8db7d6b63675658a161389317a47">
  <img src="https://a248.e.akamai.net/assets.github.com/images/modules/ajax/big_spinner_336699.gif" height="32" width="32">
</div>

    </div>

    </div>

    <!-- footer -->
    <div id="footer" >
      
  <div class="upper_footer">
     <div class="site" class="clearfix">

       <!--[if IE]><h4 id="blacktocat_ie">GitHub Links</h4><![endif]-->
       <![if !IE]><h4 id="blacktocat">GitHub Links</h4><![endif]>

       <ul class="footer_nav">
         <h4>GitHub</h4>
         <li><a href="https://github.com/about">About</a></li>
         <li><a href="https://github.com/blog">Blog</a></li>
         <li><a href="https://github.com/features">Features</a></li>
         <li><a href="https://github.com/contact">Contact &amp; Support</a></li>
         <li><a href="https://github.com/training">Training</a></li>
         <li><a href="http://status.github.com/">Site Status</a></li>
       </ul>

       <ul class="footer_nav">
         <h4>Tools</h4>
         <li><a href="http://mac.github.com/">GitHub for Mac</a></li>
         <li><a href="http://mobile.github.com/">Issues for iPhone</a></li>
         <li><a href="https://gist.github.com">Gist: Code Snippets</a></li>
         <li><a href="http://enterprise.github.com/">GitHub Enterprise</a></li>
         <li><a href="http://jobs.github.com/">Job Board</a></li>
       </ul>

       <ul class="footer_nav">
         <h4>Extras</h4>
         <li><a href="http://shop.github.com/">GitHub Shop</a></li>
         <li><a href="http://octodex.github.com/">The Octodex</a></li>
       </ul>

       <ul class="footer_nav">
         <h4>Documentation</h4>
         <li><a href="http://help.github.com/">GitHub Help</a></li>
         <li><a href="http://developer.github.com/">Developer API</a></li>
         <li><a href="http://github.github.com/github-flavored-markdown/">GitHub Flavored Markdown</a></li>
         <li><a href="http://pages.github.com/">GitHub Pages</a></li>
       </ul>

     </div><!-- /.site -->
  </div><!-- /.upper_footer -->

<div class="lower_footer">
  <div class="site" class="clearfix">
    <!--[if IE]><div id="legal_ie"><![endif]-->
    <![if !IE]><div id="legal"><![endif]>
      <ul>
          <li><a href="https://github.com/site/terms">Terms of Service</a></li>
          <li><a href="https://github.com/site/privacy">Privacy</a></li>
          <li><a href="https://github.com/security">Security</a></li>
      </ul>

      <p>&copy; 2011 <span id="_rrt" title="0.14604s from fe8.rs.github.com">GitHub</span> Inc. All rights reserved.</p>
    </div><!-- /#legal or /#legal_ie-->

      <div class="sponsor">
        <a href="http://www.rackspace.com" class="logo">
          <img alt="Dedicated Server" height="36" src="https://a248.e.akamai.net/assets.github.com/images/modules/footer/rackspace_logo.png?v2" width="38" />
        </a>
        Powered by the <a href="http://www.rackspace.com ">Dedicated
        Servers</a> and<br/> <a href="http://www.rackspacecloud.com">Cloud
        Computing</a> of Rackspace Hosting<span>&reg;</span>
      </div>
  </div><!-- /.site -->
</div><!-- /.lower_footer -->

    </div><!-- /#footer -->

    

<div id="keyboard_shortcuts_pane" class="instapaper_ignore readability-extra" style="display:none">
  <h2>Keyboard Shortcuts <small><a href="#" class="js-see-all-keyboard-shortcuts">(see all)</a></small></h2>

  <div class="columns threecols">
    <div class="column first">
      <h3>Site wide shortcuts</h3>
      <dl class="keyboard-mappings">
        <dt>s</dt>
        <dd>Focus site search</dd>
      </dl>
      <dl class="keyboard-mappings">
        <dt>?</dt>
        <dd>Bring up this help dialog</dd>
      </dl>
    </div><!-- /.column.first -->

    <div class="column middle" style=&#39;display:none&#39;>
      <h3>Commit list</h3>
      <dl class="keyboard-mappings">
        <dt>j</dt>
        <dd>Move selection down</dd>
      </dl>
      <dl class="keyboard-mappings">
        <dt>k</dt>
        <dd>Move selection up</dd>
      </dl>
      <dl class="keyboard-mappings">
        <dt>c <em>or</em> o <em>or</em> enter</dt>
        <dd>Open commit</dd>
      </dl>
      <dl class="keyboard-mappings">
        <dt>y</dt>
        <dd>Expand URL to its canonical form</dd>
      </dl>
    </div><!-- /.column.first -->

    <div class="column last" style=&#39;display:none&#39;>
      <h3>Pull request list</h3>
      <dl class="keyboard-mappings">
        <dt>j</dt>
        <dd>Move selection down</dd>
      </dl>
      <dl class="keyboard-mappings">
        <dt>k</dt>
        <dd>Move selection up</dd>
      </dl>
      <dl class="keyboard-mappings">
        <dt>o <em>or</em> enter</dt>
        <dd>Open issue</dd>
      </dl>
    </div><!-- /.columns.last -->

  </div><!-- /.columns.equacols -->

  <div style=&#39;display:none&#39;>
    <div class="rule"></div>

    <h3>Issues</h3>

    <div class="columns threecols">
      <div class="column first">
        <dl class="keyboard-mappings">
          <dt>j</dt>
          <dd>Move selection down</dd>
        </dl>
        <dl class="keyboard-mappings">
          <dt>k</dt>
          <dd>Move selection up</dd>
        </dl>
        <dl class="keyboard-mappings">
          <dt>x</dt>
          <dd>Toggle selection</dd>
        </dl>
        <dl class="keyboard-mappings">
          <dt>o <em>or</em> enter</dt>
          <dd>Open issue</dd>
        </dl>
      </div><!-- /.column.first -->
      <div class="column middle">
        <dl class="keyboard-mappings">
          <dt>I</dt>
          <dd>Mark selection as read</dd>
        </dl>
        <dl class="keyboard-mappings">
          <dt>U</dt>
          <dd>Mark selection as unread</dd>
        </dl>
        <dl class="keyboard-mappings">
          <dt>e</dt>
          <dd>Close selection</dd>
        </dl>
        <dl class="keyboard-mappings">
          <dt>y</dt>
          <dd>Remove selection from view</dd>
        </dl>
      </div><!-- /.column.middle -->
      <div class="column last">
        <dl class="keyboard-mappings">
          <dt>c</dt>
          <dd>Create issue</dd>
        </dl>
        <dl class="keyboard-mappings">
          <dt>l</dt>
          <dd>Create label</dd>
        </dl>
        <dl class="keyboard-mappings">
          <dt>i</dt>
          <dd>Back to inbox</dd>
        </dl>
        <dl class="keyboard-mappings">
          <dt>u</dt>
          <dd>Back to issues</dd>
        </dl>
        <dl class="keyboard-mappings">
          <dt>/</dt>
          <dd>Focus issues search</dd>
        </dl>
      </div>
    </div>
  </div>

  <div style=&#39;display:none&#39;>
    <div class="rule"></div>

    <h3>Issues Dashboard</h3>

    <div class="columns threecols">
      <div class="column first">
        <dl class="keyboard-mappings">
          <dt>j</dt>
          <dd>Move selection down</dd>
        </dl>
        <dl class="keyboard-mappings">
          <dt>k</dt>
          <dd>Move selection up</dd>
        </dl>
        <dl class="keyboard-mappings">
          <dt>o <em>or</em> enter</dt>
          <dd>Open issue</dd>
        </dl>
      </div><!-- /.column.first -->
    </div>
  </div>

  <div style=&#39;display:none&#39;>
    <div class="rule"></div>

    <h3>Network Graph</h3>
    <div class="columns equacols">
      <div class="column first">
        <dl class="keyboard-mappings">
          <dt><span class="badmono">←</span> <em>or</em> h</dt>
          <dd>Scroll left</dd>
        </dl>
        <dl class="keyboard-mappings">
          <dt><span class="badmono">→</span> <em>or</em> l</dt>
          <dd>Scroll right</dd>
        </dl>
        <dl class="keyboard-mappings">
          <dt><span class="badmono">↑</span> <em>or</em> k</dt>
          <dd>Scroll up</dd>
        </dl>
        <dl class="keyboard-mappings">
          <dt><span class="badmono">↓</span> <em>or</em> j</dt>
          <dd>Scroll down</dd>
        </dl>
        <dl class="keyboard-mappings">
          <dt>t</dt>
          <dd>Toggle visibility of head labels</dd>
        </dl>
      </div><!-- /.column.first -->
      <div class="column last">
        <dl class="keyboard-mappings">
          <dt>shift <span class="badmono">←</span> <em>or</em> shift h</dt>
          <dd>Scroll all the way left</dd>
        </dl>
        <dl class="keyboard-mappings">
          <dt>shift <span class="badmono">→</span> <em>or</em> shift l</dt>
          <dd>Scroll all the way right</dd>
        </dl>
        <dl class="keyboard-mappings">
          <dt>shift <span class="badmono">↑</span> <em>or</em> shift k</dt>
          <dd>Scroll all the way up</dd>
        </dl>
        <dl class="keyboard-mappings">
          <dt>shift <span class="badmono">↓</span> <em>or</em> shift j</dt>
          <dd>Scroll all the way down</dd>
        </dl>
      </div><!-- /.column.last -->
    </div>
  </div>

  <div >
    <div class="rule"></div>
    <div class="columns threecols">
      <div class="column first" >
        <h3>Source Code Browsing</h3>
        <dl class="keyboard-mappings">
          <dt>t</dt>
          <dd>Activates the file finder</dd>
        </dl>
        <dl class="keyboard-mappings">
          <dt>l</dt>
          <dd>Jump to line</dd>
        </dl>
        <dl class="keyboard-mappings">
          <dt>w</dt>
          <dd>Switch branch/tag</dd>
        </dl>
        <dl class="keyboard-mappings">
          <dt>y</dt>
          <dd>Expand URL to its canonical form</dd>
        </dl>
      </div>
    </div>
  </div>
</div>

    <div id="markdown-help" class="instapaper_ignore readability-extra">
  <h2>Markdown Cheat Sheet</h2>

  <div class="cheatsheet-content">

  <div class="mod">
    <div class="col">
      <h3>Format Text</h3>
      <p>Headers</p>
      <pre>
# This is an &lt;h1&gt; tag
## This is an &lt;h2&gt; tag
###### This is an &lt;h6&gt; tag</pre>
     <p>Text styles</p>
     <pre>
*This text will be italic*
_This will also be italic_
**This text will be bold**
__This will also be bold__

*You **can** combine them*
</pre>
    </div>
    <div class="col">
      <h3>Lists</h3>
      <p>Unordered</p>
      <pre>
* Item 1
* Item 2
  * Item 2a
  * Item 2b</pre>
     <p>Ordered</p>
     <pre>
1. Item 1
2. Item 2
3. Item 3
   * Item 3a
   * Item 3b</pre>
    </div>
    <div class="col">
      <h3>Miscellaneous</h3>
      <p>Images</p>
      <pre>
![GitHub Logo](/images/logo.png)
Format: ![Alt Text](url)
</pre>
     <p>Links</p>
     <pre>
http://github.com - automatic!
[GitHub](http://github.com)</pre>
<p>Blockquotes</p>
     <pre>
As Kanye West said:
> We're living the future so
> the present is our past.
</pre>
    </div>
  </div>
  <div class="rule"></div>

  <h3>Code Examples in Markdown</h3>
  <div class="col">
      <p>Syntax highlighting with <a href="http://github.github.com/github-flavored-markdown/" title="GitHub Flavored Markdown" target="_blank">GFM</a></p>
      <pre>
```javascript
function fancyAlert(arg) {
  if(arg) {
    $.facebox({div:'#foo'})
  }
}
```</pre>
    </div>
    <div class="col">
      <p>Or, indent your code 4 spaces</p>
      <pre>
Here is a Python code example
without syntax highlighting:

    def foo:
      if not bar:
        return true</pre>
    </div>
    <div class="col">
      <p>Inline code for comments</p>
      <pre>
I think you should use an
`&lt;addr&gt;` element here instead.</pre>
    </div>
  </div>

  </div>
</div>

    <div class="context-overlay"></div>

    
    
    
  </body>
</html>

