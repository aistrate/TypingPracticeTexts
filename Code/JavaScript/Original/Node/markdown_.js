(function( expose ) {

var Markdown = expose.Markdown = function Markdown(dialect) {
  switch (typeof dialect) {
    case "undefined":
      this.dialect = Markdown.dialects.Gruber;
      break;
    case "object":
      this.dialect = dialect;
      break;
    default:
      if (dialect in Markdown.dialects) {
        this.dialect = Markdown.dialects[dialect];
      }
      else {
        throw new Error("Unknown Markdown dialect '" + String(dialect) + "'");
      }
      break;
  }
  this.em_state = [];
  this.strong_state = [];
  this.debug_indent = "";
}

expose.parse = function( source, dialect ) {
  var md = new Markdown( dialect );
  return md.toTree( source );
}

expose.toHTML = function toHTML( source ) {
  var input = expose.toHTMLTree( source );

  return expose.renderJsonML( input );
}

expose.toHTMLTree = function toHTMLTree( input, dialect ) {
  if ( typeof input ==="string" ) input = this.parse( input, dialect );

  var attrs = extract_attr( input ),
      refs = {};

  if ( attrs && attrs.references ) {
    refs = attrs.references;
  }

  var html = convert_tree_to_html( input, refs );
  merge_text_nodes( html );
  return html;
}

var mk_block = Markdown.mk_block = function(block, trail, line) {
  if ( arguments.length == 1 ) trail = "\n\n";

  var s = new String(block);
  s.trailing = trail;
  s.toSource = function() {
    return "Markdown.mk_block( " +
            uneval(block) +
            ", " +
            uneval(trail) +
            ", " +
            uneval(line) +
            " )"
  }

  if (line != undefined)
    s.lineNumber = line;

  return s;
}

function count_lines( str ) {
  var n = 0, i = -1;;
  while ( ( i = str.indexOf('\n', i+1) ) != -1) n++;
  return n;
}

Markdown.prototype.split_blocks = function splitBlocks( input, startLine ) {
  var re = /([\s\S]+?)($|\n(?:\s*\n|$)+)/g,
      blocks = [],
      m;

  var line_no = 1;

  if ( ( m = (/^(\s*\n)/)(input) ) != null ) {
    line_no += count_lines( m[0] );
    re.lastIndex = m[0].length;
  }

  while ( ( m = re(input) ) != null ) {
    blocks.push( mk_block( m[1], m[2], line_no ) );
    line_no += count_lines( m[0] );
  }

  return blocks;
}

Markdown.prototype.processBlock = function processBlock( block, next ) {
  var cbs = this.dialect.block,
      ord = cbs.__order__;

  if ( "__call__" in cbs ) {
    return cvs.__call__.call(this, block, next);
  }

  for ( var i = 0; i < ord.length; i++ ) {
    var res = cbs[ ord[i] ].call( this, block, next );
    if ( res ) {
      if ( !res instanceof Array || ( res.length > 0 && !( res[0] instanceof Array ) ) )
        this.debug(ord[i], "didn't return a proper array");
      return res;
    }
  }

  return [];
}

Markdown.prototype.processInline = function processInline( block ) {
  return this.dialect.inline.__call__.call( this, String( block ) );
}

Markdown.prototype.toTree = function toTree( source, custom_root ) {
  var blocks = source instanceof Array
             ? source
             : this.split_blocks( source );

  var old_tree = this.tree;
  try {
    this.tree = custom_root || this.tree || [ "markdown" ];

    blocks:
    while ( blocks.length ) {
      var b = this.processBlock( blocks.shift(), blocks );

      if ( !b.length ) continue blocks;

      this.tree.push.apply( this.tree, b );
    }
    return this.tree;
  }
  finally {
    if ( custom_root )
      this.tree = old_tree;
  }

}

Markdown.prototype.debug = function () {
  var args = Array.prototype.slice.call( arguments);
  args.unshift(this.debug_indent);
  print.apply( print, args );
}

Markdown.prototype.loop_re_over_block = function( re, block, cb ) {
  var m,
      b = block.valueOf();

  while ( b.length && (m = re(b) ) != null) {
    b = b.substr( m[0].length );
    cb.call(this, m);
  }
  return b;
}

Markdown.dialects = {};

Markdown.dialects.Gruber = {
  block: {
    atxHeader: function atxHeader( block, next ) {
      var m = block.match( /^(#{1,6})\s*(.*?)\s*#*\s*(?:\n|$)/ );

      if ( !m ) return undefined;

      var header = [ "header", { level: m[ 1 ].length }, m[ 2 ] ];

      if ( m[0].length < block.length )
        next.unshift( mk_block( block.substr( m[0].length ), block.trailing, block.lineNumber + 2 ) );

      return [ header ];
    },

    setextHeader: function setextHeader( block, next ) {
      var m = block.match( /^(.*)\n([-=])\2\2+(?:\n|$)/ );

      if ( !m ) return undefined;

      var level = ( m[ 2 ] === "=" ) ? 1 : 2;
      var header = [ "header", { level : level }, m[ 1 ] ];

      if ( m[0].length < block.length )
        next.unshift( mk_block( block.substr( m[0].length ), block.trailing, block.lineNumber + 2 ) );

      return [ header ];
    },

    code: function code( block, next ) {

      var ret = [],
          re = /^(?: {0,3}\t| {4})(.*)\n?/,
          lines;

      var m = block.match( re );

      if ( !m ) return undefined;

      block_search:
      do {
        var b = this.loop_re_over_block(
                  re, block.valueOf(), function( m ) { ret.push( m[1] ) } );

        if (b.length) {
          next.unshift( mk_block(b, block.trailing) );
          break block_search;
        }
        else if (next.length) {
          var m = next[0].match( re );

          if ( !m ) break block_search;

          ret.push ( block.trailing.replace(/[^\n]/g, '').substring(2) );

          block = next.shift();
        }
        else
          break block_search;
      } while (true);

      return [ [ "code_block", ret.join("\n") ] ];
    },

    horizRule: function horizRule( block, next ) {
      var m = block.match( /^(?:([\s\S]*?)\n)?[ \t]*([-_*])(?:[ \t]*\2){2,}[ \t]*(?:\n([\s\S]*))?$/ );

      if ( !m ) {
        return undefined;
      }

      var jsonml = [ [ "hr" ] ];

      if ( m[ 1 ] ) {
        jsonml.unshift.apply( jsonml, this.processBlock( m[ 1 ], [] ) );
      }

      if ( m[ 3 ] ) {
        next.unshift( mk_block( m[ 3 ] ) );
      }

      return jsonml;
    },

    lists: (function( ) {
      var any_list = "[*+-]|\\d\\.",
          bullet_list = /[*+-]/,
          number_list = /\d+\./,
          is_list_re = new RegExp( "^( {0,3})(" + any_list + ")[ \t]+" ),
          indent_re = "(?: {0,3}\\t| {4})";

      function regex_for_depth( depth ) {

        return new RegExp(
          "(?:^(" + indent_re + "{0," + depth + "} {0,3})(" + any_list + ")\\s+)|" +
          "(^" + indent_re + "{0," + (depth-1) + "}[ ]{0,4})"
        );
      }
      function expand_tab( input ) {
        return input.replace( / {0,3}\t/g, "    " );
      }

      function add(li, loose, inline, nl) {
        if (loose) {
            li.push( [ "para" ].concat(inline) );
          return;
        }
        var add_to = li[li.length -1] instanceof Array && li[li.length - 1][0] == "para"
                   ? li[li.length -1]
                   : li;

        if (nl && li.length > 1) inline.unshift(nl);

        for (var i=0; i < inline.length; i++) {
          var what = inline[i],
              is_str = typeof what == "string";
          if (is_str && add_to.length > 1 && typeof add_to[add_to.length-1] == "string" )
          {
            add_to[ add_to.length-1 ] += what;
          }
          else {
            add_to.push( what );
          }
        }
      }

      function get_contained_blocks( depth, blocks ) {

        var re = new RegExp( "^(" + indent_re + "{" + depth + "}.*?\\n?)*$" ),
            replace = new RegExp("^" + indent_re + "{" + depth + "}", "gm"),
            ret = [];

        while ( blocks.length > 0 ) {
          if ( re( blocks[0] ) ) {
            var b = blocks.shift(),
                x = b.replace( replace, "");

            ret.push( mk_block( x, b.trailing, b.lineNumber ) );
          }
          break;
        }
        return ret;
      }

      function paragraphify(s, i, stack) {
        var list = s.list;
        var last_li = list[list.length-1];

        if (last_li[1] instanceof Array && last_li[1][0] == "para") {
          return;
        }
        if (i+1 == stack.length) {
          last_li.push( ["para"].concat( last_li.splice(1) ) );
        }
        else {
          var sublist = last_li.pop();
          last_li.push( ["para"].concat( last_li.splice(1) ), sublist );
        }
      }

      return function( block, next ) {
        var m = block.match( is_list_re );
        if ( !m ) return undefined;

        function make_list( m ) {
          var list = bullet_list( m[2] )
                   ? ["bulletlist"]
                   : ["numberlist"];

          stack.push( { list: list, indent: m[1] } );
          return list;
        }

        var stack = [],
            list = make_list( m ),
            last_li,
            loose = false,
            ret = [ stack[0].list ];

        loose_search:
        while( true ) {
          var lines = block.split( /(?=\n)/ );

          var li_accumulate = "";

          tight_search:
          for (var line_no=0; line_no < lines.length; line_no++) {
            var nl = "",
                l = lines[line_no].replace(/^\n/, function(n) { nl = n; return "" });

            var line_re = regex_for_depth( stack.length );

            m = l.match( line_re );

            if ( m[1] !== undefined ) {
              if ( li_accumulate.length ) {
                add( last_li, loose, this.processInline( li_accumulate ), nl );
                loose = false;
                li_accumulate = "";
              }

              m[1] = expand_tab( m[1] );
              var wanted_depth = Math.floor(m[1].length/4)+1;
              if ( wanted_depth > stack.length ) {
                list = make_list( m );
                last_li.push( list );
                last_li = list[1] = [ "listitem" ];
              }
              else {
                var found = stack.some(function(s, i) {
                  if ( s.indent != m[1] ) return false;
                  list = s.list;
                  stack.splice(i+1);
                  return true;
                });

                if (!found) {
                  wanted_depth++;
                  if (wanted_depth <= stack.length) {
                    stack.splice(wanted_depth);
                    list = stack[wanted_depth-1].list;
                  }
                  else {
                    list = make_list(m);
                    last_li.push(list);
                  }
                }

                last_li = [ "listitem" ];
                list.push(last_li);
              }
              nl = "";
            }

            if (l.length > m[0].length) {
              li_accumulate += nl + l.substr( m[0].length );
            }
          }

          if ( li_accumulate.length ) {
            add( last_li, loose, this.processInline( li_accumulate ), nl );
            loose = false;
            li_accumulate = "";
          }

          var contained = get_contained_blocks( stack.length, next );

          if (contained.length > 0) {
            stack.forEach( paragraphify, this );

            last_li.push.apply( last_li, this.toTree( contained, [] ) );
          }

          var next_block = next[0] && next[0].valueOf() || "";

          if ( next_block.match(is_list_re) || next_block.match( /^ / ) ) {
            block = next.shift();

            var hr = this.dialect.block.horizRule( block, next );

            if (hr) {
              ret.push.apply(ret, hr);
              break;
            }

            stack.forEach( paragraphify , this );

            loose = true;
            continue loose_search;
          }
          break;
        }

        return ret;
      }
    })(),

    blockquote: function blockquote( block, next ) {
      if ( !block.match( /^>/m ) )
        return undefined;

      var jsonml = [];

      if ( block[ 0 ] != ">" ) {
        var lines = block.split( /\n/ ),
            prev = [];

        while ( lines.length && lines[ 0 ][ 0 ] != ">" ) {
            prev.push( lines.shift() );
        }

        block = lines.join( "\n" );
        jsonml.push.apply( jsonml, this.processBlock( prev.join( "\n" ), [] ) );
      }

      while ( next.length && next[ 0 ][ 0 ] == ">" ) {
        var b = next.shift();
        block += block.trailing + b;
        block.trailing = b.trailing;
      }

      var input = block.replace( /^> ?/gm, '' ),
          old_tree = this.tree;
      jsonml.push( this.toTree( input, [ "blockquote" ] ) );

      return jsonml;
    },

    referenceDefn: function referenceDefn( block, next) {
      var re = /^\s*\[(.*?)\]:\s*(\S+)(?:\s+(?:(['"])(.*?)\3|\((.*?)\)))?\n?/;

      if ( !block.match(re) )
        return undefined;

      if ( !extract_attr( this.tree ) ) {
        this.tree.splice( 1, 0, {} );
      }

      var attrs = extract_attr( this.tree );

      if ( attrs.references === undefined ) {
        attrs.references = {};
      }

      var b = this.loop_re_over_block(re, block, function( m ) {

        if ( m[2] && m[2][0] == '<' && m[2][m[2].length-1] == '>' )
          m[2] = m[2].substring( 1, m[2].length - 1 );

        var ref = attrs.references[ m[1].toLowerCase() ] = {
          href: m[2]
        };

        if (m[4] !== undefined)
          ref.title = m[4];
        else if (m[5] !== undefined)
          ref.title = m[5];

      } );

      if (b.length)
        next.unshift( mk_block( b, block.trailing ) );

      return [];
    },

    para: function para( block, next ) {
      return [ ["para"].concat( this.processInline( block ) ) ];
    }
  }
}

Markdown.dialects.Gruber.inline = {
    __call__: function inline( text, patterns ) {
      var out = [ ],
          m,
          re = new RegExp( "([\\s\\S]*?)(" + (patterns.source || patterns) + ")", "g" ),
          lastIndex = 0;

      function add(x) {
        if (typeof x == "string" && typeof out[out.length-1] == "string")
          out[ out.length-1 ] += x;
        else
          out.push(x);
      }

      while ( ( m = re.exec(text) ) != null) {
        if ( m[1] ) add( m[1] );
        else        m[1] = { length: 0 };

        var res;
        if ( m[2] in this.dialect.inline ) {
          res = this.dialect.inline[ m[2] ].call(
                    this,
                    text.substr( m.index + m[1].length ), m, out );
        }
        res = res || [ m[2].length, m[2] ];

        var len = res.shift();
        re.lastIndex += ( len - m[2].length );

        res.forEach(add);

        lastIndex = re.lastIndex;
      }

      if ( text.length > lastIndex )
        add( text.substr( lastIndex ) );

      return out;
    },

    "\\": function escaped( text ) {
      if ( text.match( /^\\[\\`\*_{}\[\]()#\+.!\-]/ ) )
        return [ 2, text[1] ];
      else
        return [ 1, "\\" ];
    },

    "![": function image( text ) {
      var m = text.match( /^!\[(.*?)\][ \t]*\([ \t]*(\S*)(?:[ \t]+(["'])(.*?)\3)?[ \t]*\)/ );

      if ( m ) {
        if ( m[2] && m[2][0] == '<' && m[2][m[2].length-1] == '>' )
          m[2] = m[2].substring( 1, m[2].length - 1 );

        m[2] == this.dialect.inline.__call__.call( this, m[2], /\\/ )[0];

        var attrs = { alt: m[1], href: m[2] || "" };
        if ( m[4] !== undefined)
          attrs.title = m[4];

        return [ m[0].length, [ "img", attrs ] ];
      }

      m = text.match( /^!\[(.*?)\][ \t]*\[(.*?)\]/ );

      if ( m ) {
        return [ m[0].length, [ "img_ref", { alt: m[1], ref: m[2].toLowerCase(), text: m[0] } ] ];
      }

      return [ 2, "![" ];
    },

    "[": function link( text ) {
      var m = text.match( /^\[([\s\S]*?)\][ \t]*\([ \t]*(\S+)(?:[ \t]+(["'])(.*?)\3)?[ \t]*\)/ );

      if ( m ) {
        if ( m[2] && m[2][0] == '<' && m[2][m[2].length-1] == '>' )
          m[2] = m[2].substring( 1, m[2].length - 1 );

        m[2] = this.dialect.inline.__call__.call( this, m[2], /\\/ )[0];

        var attrs = { href: m[2] || "" };
        if ( m[4] !== undefined)
          attrs.title = m[4];

        return [ m[0].length, [ "link", attrs, m[1] ] ];
      }

      m = text.match( /^\[([\s\S]*?)\](?: ?\[(.*?)\])?/ );

      if ( m ) {
        if ( m[2] === undefined || m[2] === "" ) m[2] = m[1];

        return [
          m[ 0 ].length,
          [
            "link_ref",
            {
              ref: m[ 2 ].toLowerCase(),
              original: m[ 0 ]
            },
            m[ 1 ]
          ]
        ];
      }

      return [ 1, "[" ];
    },

    "<": function autoLink( text ) {
      var m;

      if ( ( m = text.match( /^<(?:((https?|ftp|mailto):[^>]+)|(.*?@.*?\.[a-zA-Z]+))>/ ) ) != null ) {
        if ( m[3] ) {
          return [ m[0].length, [ "link", { href: "mailto:" + m[3] }, m[3] ] ];

        }
        else if ( m[2] == "mailto" ) {
          return [ m[0].length, [ "link", { href: m[1] }, m[1].substr("mailto:".length ) ] ];
        }
        else
          return [ m[0].length, [ "link", { href: m[1] }, m[1] ] ];
      }

      return [ 1, "<" ];
    },

    "`": function inlineCode( text ) {
      var m = text.match( /(`+)(([\s\S]*?)\1)/ );

      if ( m && m[2] )
        return [ m[1].length + m[2].length, [ "inlinecode", m[3] ] ];
      else {
        return [ 1, "`" ];
      }
    },

    "  \n": function lineBreak( text ) {
      return [ 3, [ "linebreak" ] ];
    }

}

function strong_em( tag, md ) {

  var state_slot = tag + "_state",
      other_slot = tag == "strong" ? "em_state" : "strong_state";

  function CloseTag(len) {
    this.len_after = len;
    this.name = "close_" + md;
  }

  return function ( text, orig_match ) {

    if (this[state_slot][0] == md) {
      this[state_slot].shift();

      return[ text.length, new CloseTag(text.length-md.length) ];
    }
    else {
      var other = this[other_slot].slice(),
          state = this[state_slot].slice();

      this[state_slot].unshift(md);

      var res = this.processInline( text.substr( md.length ) );

      var last = res[res.length - 1];

      var check = this[state_slot].shift();
      if (last instanceof CloseTag) {
        res.pop();
        var consumed = text.length - last.len_after;
        return [ consumed, [ tag ].concat(res) ];
      }
      else {
        this[other_slot] = other;
        this[state_slot] = state;

        return [ md.length, md ];
      }
    }
  }
}

Markdown.dialects.Gruber.inline["**"] = strong_em("strong", "**");
Markdown.dialects.Gruber.inline["__"] = strong_em("strong", "__");
Markdown.dialects.Gruber.inline["*"]  = strong_em("em", "*");
Markdown.dialects.Gruber.inline["_"]  = strong_em("em", "_");

Markdown.buildBlockOrder = function(d) {
  var ord = [];
  for ( var i in d ) {
    if ( i == "__order__" || i == "__call__" ) continue;
    ord.push( i );
  }
  d.__order__ = ord;
}

Markdown.buildInlinePatterns = function(d) {
  var patterns = [];

  for ( var i in d ) {
    if (i == "__call__") continue;
    var l = i.replace( /([\\.*+?|()\[\]{}])/g, "\\$1" )
             .replace( /\n/, "\\n" );
    patterns.push( i.length == 1 ? l : "(?:" + l + ")" );
  }

  patterns = patterns.join("|");

  var fn = d.__call__;
  d.__call__ = function(text, pattern) {
    if (pattern != undefined)
      return fn.call(this, text, pattern);
    else
      return fn.call(this, text, patterns);
  }
}

Markdown.subclassDialect = function( d ) {
  function Block() {};
  Block.prototype = d.block;
  function Inline() {};
  Inline.prototype = d.inline;

  return { block: new Block(), inline: new Inline() };
}

Markdown.buildBlockOrder ( Markdown.dialects.Gruber.block );
Markdown.buildInlinePatterns( Markdown.dialects.Gruber.inline );

Markdown.dialects.Maruku = Markdown.subclassDialect( Markdown.dialects.Gruber );

Markdown.dialects.Maruku.block.document_meta = function document_meta( block, next ) {
  if ( block.lineNumber > 1 ) return undefined;

  if ( ! block.match( /^(?:\w+:.*\n)*\w+:.*$/ ) ) return undefined;

  if ( !extract_attr( this.tree ) ) {
    this.tree.splice( 1, 0, {} );
  }

  var pairs = block.split( /\n/ );
  for ( p in pairs ) {
    var m = pairs[ p ].match( /(\w+):\s*(.*)$/ ),
        key = m[ 1 ].toLowerCase(),
        value = m[ 2 ];

    this.tree[ 1 ][ key ] = value;
  }

  return [];
}

Markdown.dialects.Maruku.block.block_meta = function block_meta( block, next ) {
  var m = block.match( /(^|\n) {0,3}\{:\s*((?:\\\}|[^\}])*)\s*\}$/ );
  if ( !m ) return undefined;

  var attr = process_meta_hash( m[ 2 ] );

  if ( m[ 1 ] === "" ) {
    var node = this.tree[ this.tree.length - 1 ],
        hash = extract_attr( node );

    if ( typeof node === "string" ) return undefined;

    if ( !hash ) {
      hash = {};
      node.splice( 1, 0, hash );
    }

    for ( a in attr ) {
      hash[ a ] = attr[ a ];
    }

    return [];
  }

  var b = block.replace( /\n.*$/, "" ),
      result = this.processBlock( b, [] );

  var hash = extract_attr( result[ 0 ] );
  if ( !hash ) {
    hash = {};
    result[ 0 ].splice( 1, 0, hash );
  }

  for ( a in attr ) {
    hash[ a ] = attr[ a ];
  }

  return result;
}

Markdown.dialects.Maruku.block.definition_list = function definition_list( block, next ) {
  var tight = /^((?:[^\s:].*\n)+):\s+([^]+)$/,
      list = [ "dl" ];

  if ( ( m = block.match( tight ) ) ) {
    var blocks = [ block ];
    while ( next.length && tight.exec( next[ 0 ] ) ) {
      blocks.push( next.shift() );
    }

    for ( var b = 0; b < blocks.length; ++b ) {
      var m = blocks[ b ].match( tight ),
          terms = m[ 1 ].replace( /\n$/, "" ).split( /\n/ ),
          defns = m[ 2 ].split( /\n:\s+/ );

      for ( var i = 0; i < terms.length; ++i ) {
        list.push( [ "dt", terms[ i ] ] );
      }

      for ( var i = 0; i < defns.length; ++i ) {
        list.push( [ "dd" ].concat( this.processInline( defns[ i ].replace( /(\n)\s+/, "$1" ) ) ) );
      }
    }
  }
  else {
    return undefined;
  }

  return [ list ];
}

Markdown.dialects.Maruku.inline[ "{:" ] = function inline_meta( text, matches, out ) {
  if ( !out.length ) {
    return [ 2, "{:" ];
  }

  var before = out[ out.length - 1 ];

  if ( typeof before === "string" ) {
    return [ 2, "{:" ];
  }

  var m = text.match( /^\{:\s*((?:\\\}|[^\}])*)\s*\}/ );

  if ( !m ) {
    return [ 2, "{:" ];
  }

  var meta = process_meta_hash( m[ 1 ] ),
      attr = extract_attr( before );

  if ( !attr ) {
    attr = {};
    before.splice( 1, 0, attr );
  }

  for ( var k in meta ) {
    attr[ k ] = meta[ k ];
  }

  return [ m[ 0 ].length, "" ];
}

Markdown.buildBlockOrder ( Markdown.dialects.Maruku.block );
Markdown.buildInlinePatterns( Markdown.dialects.Maruku.inline );

function extract_attr( jsonml ) {
  return jsonml instanceof Array
      && jsonml.length > 1
      && typeof jsonml[ 1 ] === "object"
      && !( jsonml[ 1 ] instanceof Array )
      ? jsonml[ 1 ]
      : undefined;
}

function process_meta_hash( meta_string ) {
  var meta = split_meta_hash( meta_string ),
      attr = {};

  for ( var i = 0; i < meta.length; ++i ) {
    if ( /^#/.test( meta[ i ] ) ) {
      attr.id = meta[ i ].substring( 1 );
    }
    else if ( /^\./.test( meta[ i ] ) ) {
      if ( attr['class'] ) {
        attr['class'] = attr['class'] + meta[ i ].replace( /./, " " );
      }
      else {
        attr['class'] = meta[ i ].substring( 1 );
      }
    }
    else if ( /=/.test( meta[ i ] ) ) {
      var s = meta[ i ].split( /=/ );
      attr[ s[ 0 ] ] = s[ 1 ];
    }
  }

  return attr;
}

function split_meta_hash( meta_string ) {
  var meta = meta_string.split( "" ),
      parts = [ "" ],
      in_quotes = false;

  while ( meta.length ) {
    var letter = meta.shift();
    switch ( letter ) {
      case " " :
        if ( in_quotes ) {
          parts[ parts.length - 1 ] += letter;
        }
        else {
          parts.push( "" );
        }
        break;
      case "'" :
      case '"' :
        in_quotes = !in_quotes;
        break;
      case "\\" :
        letter = meta.shift();
      default :
        parts[ parts.length - 1 ] += letter;
        break;
    }
  }

  return parts;
}

expose.renderJsonML = function( jsonml, options ) {
  options = options || {};
  options.root = options.root || false;
  options.xhtml = options.xhtml || false;

  var content = [];

  if ( options.root ) {
    content.push( render_tree( jsonml, options.xhtml ) );
  }
  else {
    jsonml.shift();
    if ( jsonml.length && typeof jsonml[ 0 ] === "object" && !( jsonml[ 0 ] instanceof Array ) ) {
      jsonml.shift();
    }

    while ( jsonml.length ) {
      content.push( render_tree( jsonml.shift(), options.xhtml ) );
    }
  }

  return content.join( "\n\n" );
}

function render_tree( jsonml, xhtml ) {
  if ( typeof jsonml === "string" ) {
    return jsonml.replace( /&/g, "&amp;" )
                 .replace( /</g, "&lt;" )
                 .replace( />/g, "&gt;" );
  }

  var tag = jsonml.shift(),
      attributes = {},
      content = [];

  if ( jsonml.length && typeof jsonml[ 0 ] === "object" && !( jsonml[ 0 ] instanceof Array ) ) {
    attributes = jsonml.shift();
  }

  while ( jsonml.length ) {
    content.push( arguments.callee( jsonml.shift(), xhtml ) );
  }

  var tag_attrs = "";
  for ( var a in attributes ) {
    tag_attrs += " " + a + '="' + attributes[ a ] + '"';
  }
  
  var markup = "<"+ tag + tag_attrs;
  var contentstr = content.join( "" );
  if ( xhtml && contentstr.length == 0 ) {
    markup +=  " />";
  }
  else {
    markup += ">" + contentstr + "</" + tag + ">";
  }
  return markup;
}

function convert_tree_to_html( tree, references ) {
  var jsonml = tree.slice( 0 );

  var attrs = extract_attr( jsonml );
  if ( attrs ) {
    jsonml[ 1 ] = {};
    for ( var i in attrs ) {
      jsonml[ 1 ][ i ] = attrs[ i ];
    }
    attrs = jsonml[ 1 ];
  }

  if ( typeof jsonml === "string" ) {
    return jsonml;
  }

  switch ( jsonml[ 0 ] ) {
    case "header":
      jsonml[ 0 ] = "h" + jsonml[ 1 ].level;
      delete jsonml[ 1 ].level;
      break;
    case "bulletlist":
      jsonml[ 0 ] = "ul";
      break;
    case "numberlist":
      jsonml[ 0 ] = "ol";
      break;
    case "listitem":
      jsonml[ 0 ] = "li";
      break;
    case "para":
      jsonml[ 0 ] = "p";
      break;
    case "markdown":
      jsonml[ 0 ] = "html";
      if ( attrs ) delete attrs.references;
      break;
    case "code_block":
      jsonml[ 0 ] = "pre";
      var i = attrs ? 2 : 1;
      var code = [ "code" ];
      code.push.apply( code, jsonml.splice( i ) );
      jsonml[ i ] = code;
      break;
    case "inlinecode":
      jsonml[ 0 ] = "code";
      break;
    case "img":
      jsonml[ 1 ].src = jsonml[ 1 ].href;
      delete jsonml[ 1 ].href;
      break;
    case "linebreak":
      jsonml[0] = "br";
    break;
    case "link":
      jsonml[ 0 ] = "a";
      break;
    case "link_ref":
      jsonml[ 0 ] = "a";

      var ref = references[ attrs.ref ];

      if ( ref ) {
        delete attrs.ref;

        attrs.href = ref.href;
        if ( ref.title ) {
          attrs.title = ref.title;
        }

        delete attrs.original;
      }
      else {
        return attrs.original;
      }
      break;
  }

  var i = 1;

  if ( attrs ) {
    for ( var key in jsonml[ 1 ] ) {
      i = 2;
    }
    if ( i === 1 ) {
      jsonml.splice( i, 1 );
    }
  }

  for ( ; i < jsonml.length; ++i ) {
    jsonml[ i ] = arguments.callee( jsonml[ i ], references );
  }

  return jsonml;
}

function merge_text_nodes( jsonml ) {
  var i = extract_attr( jsonml ) ? 2 : 1;

  while ( i < jsonml.length ) {
    if ( typeof jsonml[ i ] === "string" ) {
      if ( i + 1 < jsonml.length && typeof jsonml[ i + 1 ] === "string" ) {
        jsonml[ i ] += jsonml.splice( i + 1, 1 )[ 0 ];
      }
      else {
        ++i;
      }
    }
    else {
      arguments.callee( jsonml[ i ] );
      ++i;
    }
  }
}

})(exports);
