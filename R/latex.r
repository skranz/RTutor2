unicode.math = function(txt, math.start = "\\$",len.math.start=1, math.end = "\\$", len.math.end=1, inner = "[\\\\_.\\{\\}0-9a-zA-Z /\\-\\+\\*\\^\\(\\)=<>]*?", block.start = "", block.end="", replace.equation.array=TRUE) {
  restore.point("unicode.math")

  if (length(txt)>0) txt = merge.lines(txt)

  #regexec(paste0(math.start), txt)
  #regexec(paste0(math.start,inner,math.end), txt)

  pattern = paste0(math.start,inner, math.end)
  pos = str.find(txt,pattern,fixed=FALSE)

  if (NROW(pos)==0) return(txt)

  maths = substring(txt, pos[,1]+len.math.start,pos[,2]-len.math.end)

  rmaths = sapply(maths, replace.latex.with.unicode)
  if (replace.equation.array)
    rmaths = sapply(rmaths, replace.latex.equation.array)

  keep = grepl("\\\\",rmaths)
  change = which(!keep)
  if (length(change)==0) return(txt)

  str = paste0(maths[change], collapse="\nbReAk\n")

  str = replace.latex.with.unicode(str)

  li = find.subscripts(str)$s
  if (length(li)>1) {
    sub.ind = seq(2,length(li),by=2)
    var.ind = sub.ind-1
    str = paste0(collapse="",
      li[var.ind],'<sub>',li[sub.ind],'</sub>'
    )
    if (max(sub.ind)<length(li))
      str = paste0(str,li[length(li)])
  }
  trans = sep.lines(str,collapse="\nbReAk\n")
  trans = paste0(block.start, trans, block.end)
  txt = str.replace.at.pos(txt, pos[change,,drop=FALSE],new = trans)

  txt
}

unicode.rmd.math = function(txt) {
  restore.point("unicode.rmd.math")
  txt = unicode.math(txt, math.start = "\\$", math.end = "\\$", block.start="<mathinline>", block.end="</mathinline>")
  txt = unicode.math(txt, math.start = "\\$\\$", math.end = "\\$\\$",len.math.start=2, len.math.end=2, block.start="<mathblock>", block.end="</mathblock>")
  txt = unicode.math(txt, math.start = "\\Q\\[\\E", math.end = "\\Q\\]\\E",len.math.start=2, len.math.end=2, block.start="<mathblock>", block.end="</mathblock>")
  txt
}

replace.latex.equation.array = function(str) {

  #str = "\\begin{eqnarray*} (1+\\pi_t) & = & (1+E\\pi_t) & \\cdot (1+\\mu) \\cdot ws(Y_t ) & \\end{eqnarray*}"
  if (!grepl("\\begin{eqnarray*}",str, fixed=TRUE)) return(str)
  restore.point("replace.latex.equation.array")

  str = gsub("\\begin{eqnarray*}","<table><tr><td>", str, fixed=TRUE)
  str = gsub("\\end{eqnarray*}","</td></tr></table>", str, fixed=TRUE)
  str = gsub("&","</td><td>", str, fixed=TRUE)
  str = gsub("\\\\\\\\","</td></tr><tr><td>", str, fixed=TRUE)
  str = gsub("\\\\","</td></tr><tr><td>", str, fixed=TRUE)
  str


}

examples.unicode.math = function() {
  txt = "Annahme: Zentralbank kann direkt Nominalzins \\(i_t \\geq 0\\) festlegen."
  txt = "Auch bei einem Rückgang von \\(Y_1=110\\) auf \\(Y_{NAI}\\) bleiben die Inflationserwartungen auf dem Niveau"
  unicode.html.math(txt)

  txt = unicode.math(txt, math.start = "\\Q\\(\\E", math.end = "\\Q\\)\\E",len.math.start=2, len.math.end=2)
  txt = unicode.math(txt, math.start = "\\\\\\(", math.end = "\\\\\\)", len.math.start=2, len.math.end=2)
}

unicode.html.math = function(txt) {
  restore.point("unicode.html.math")
  txt = unicode.math(txt, math.start = "\\Q\\(\\E", math.end = "\\Q\\)\\E",len.math.start=2, len.math.end=2, block.start="<mathinline>", block.end="</mathinline>")
  txt = unicode.math(txt, math.start = "\\Q\\[\\E", math.end = "\\Q\\]\\E",len.math.start=2, len.math.end=2, block.start="<mathblock>", block.end="</mathblock>")
  txt
}

math.css = function() {
'
mathinline {
  display: inline;
  font-family: "Georgia", serif;
  font-weight: 450;
  font-size: 1.1em;
  font-style: italic;
}
mathblock {
  padding-top: 5px;
  padding-bottom: 5px;
  display: block;
  font-family: "Georgia", serif;
  font-size: 1.25em;
  font-weight: 450;
  text-align: center;
  font-style: italic;
}

  '
}

find.subscripts = function(str) {
  restore.point("find.subscripts")


  # remove curley braces
  str = gsub("{{","jJj",str, fixed=TRUE)
  str = gsub("}}","hHh",str, fixed=TRUE)


    str = gsub("{","",str, fixed=TRUE)
  str = gsub("}"," ",str, fixed=TRUE)
  str = gsub("  "," ",str, fixed=TRUE)



  # find subscripts
  pos = str.find(str,'_[0-9a-zA-Z_|.=\\{\\}\\-\\+\\*\\°]+',fixed=FALSE)
  if (NROW(pos)==0) {
    return(list(s=str,is.sub=FALSE))
  }

  spl = str.split.at.pos(str,pos,keep.pos = TRUE)
  first = pos[1,1]==1
  if (first) {
    is.sub = rep(c(TRUE,FALSE),length.out=length(spl))
  } else {
    is.sub = rep(c(FALSE,TRUE),length.out=length(spl))
  }
  spl[is.sub] = substring(spl[is.sub],2)



  list(s=spl, is.sub=is.sub)

}

replace.latex.with.unicode = function(str) {

  latex = c( "\\alpha","\\beta","\\gamma","\\delta","\\epsilon","\\zeta","\\eta","\\theta","\\iota","\\kappa","\\lambda","\\mu","\\nu","\\xi","\\pi","\\rho","\\varsigma","\\sigma","\\tau","\\upsilon","\\phi","\\chi","\\psi","\\omega","\\Gamma","\\Delta","\\Theta","\\Lambda","\\Xi","\\Pi","\\Sigma","\\Upsilon","\\Phi","\\Psi","\\Omega","\\neg","\\pm","\\cdot","\\to","\\Rightarrow","\\Leftrightarrow","\\forall","\\partial","\\exists","\\emptyset","\\nabla","\\in","\\notin","\\prod","\\sum","\\surd","\\infty","\\wedge","\\vee","\\cap","\\cup","\\int","\\approx","\\neq","\\equiv","\\leq","\\geq","\\subset","\\supset","\\^circ","\\times","\\lfloor","\\rfloor","\\lceil","\\rceil",
  "\\left","\\right",
"\\varepsilon","\\eps")

uc = c( "\U3B1","\U3B2","\U3B3","\U3B4","\U3B5","\U3B6","\U3B7","\U3B8","\U3B9","\U3BA","\U3BB","\U3BC","\U3BD","\U3BE","\U3C0","\U3C1","\U3C2","\U3C3","\U3C4","\U3C5","\U3C6","\U3C7","\U3C8","\U3C9","\U393","\U394","\U398","\U39B","\U39E","\U3A0","\U3A3","\U3A5","\U3A6","\U3A8","\U3A9","\U00AC","\U00B1","\U00B7","\U2192","\U21D2","\U21D4","\U2200","\U2202","\U2203","\U2205","\U2207","\U2208","\U2209","\U220F","\U2211","\U221A","\U221E","\U2227","\U2228","\U2229","\U222A","\U222B","\U2248","\U2260","\U2261","\U2264","\U2265","\U2282","\U2283","\U00B0","\U00D7","\U230A","\U230B","\U2308","\U2309",
"","",
"\U0395","\U0395")

  pos = str.find(str,'\\\\[0-9a-zA-Z]+',fixed=FALSE)
  spl = str.split.at.pos(str,pos,keep.pos = TRUE)
  ind = match(spl, latex)
  rows = !is.na(ind)
  spl[rows] = uc[ind[rows]]



  res = paste0(spl,collapse="")
  Encoding(res) = "UTF-8"
  res
}

make.greece.code = function() {
  str='
  α,alpha,&alpha;,x3B1
  β,beta,&beta;,x3B2
  γ,gamma,&gamma;,x3B3
  δ,delta,&delta;,x3B4
  ε,epsilon,&epsilon;,x3B5
  ζ,zeta,&zeta;,x3B6
  η,eta,&eta;,x3B7
  θ,theta,&theta;,x3B8
  ι,iota,&iota;,x3B9
  κ,kappa,&kappa;,x3BA
  λ,lambda,&lambda;,x3BB
  μ,mu,&mu;,x3BC
  ν,nu,&nu;,x3BD
  ξ,xi,&xi;,x3BE
  π,pi,&pi;,x3C0
  ρ,rho,&rho;,x3C1
  ς,varsigma,&sigmaf;,x3C2
  σ,sigma,&sigma;,x3C3
  τ,tau,&tau;,x3C4
  υ,upsilon,&upsilon;,x3C5
  φ,phi,&phi;,x3C6
  χ,chi,&chi;,x3C7
  ψ,psi,&psi;,x3C8
  ω,omega,&omega;,x3C9
  Γ,Gamma,&Gamma;,x393
  Δ,Delta,&Delta;,x394
  Θ,Theta,&Theta;,x398
  Λ,Lambda,&Lambda;,x39B
  Ξ,Xi,&Xi;,x39E
  Π,Pi,&Pi;,x3A0
  Σ,Sigma,&Sigma;,x3A3
  Υ,Upsilon,&Upsilon;,x3A5
  Φ,Phi,&Phi;,x3A6
  Ψ,Psi,&Psi;,x3A8
  Ω,Omega,&Omega;,x3A9
¬,neg,&not;,x00AC
±,pm,&plusmn;,x00B1
·,cdot,&middot;,x00B7
→,to,&rarr;,x2192
⇒,Rightarrow,&rArr;,x21D2
⇔,Leftrightarrow,&hArr;,x21D4
∀,forall,&forall;,x2200
∂,partial,&part;,x2202
∃,exists,&exist;,x2203
∅,emptyset,&empty;,x2205
∇,nabla,&nabla;,x2207
∈,in,&isin;,x2208
∉,notin,&notin;,x2209
∏,prod,&prod;,x220F
∑,sum,&sum;,x2211
√,surd,&radic;,x221A
∞,infty,&infin;,x221E
∧,wedge,&and;,x2227
∨,vee,&or;,x2228
∩,cap,&cap;,x2229
∪,cup,&cup;,x222A
∫,int,&int;,x222B
≈,approx,&asymp;,x2248
≠,neq,&ne;,x2260
≡,equiv,&equiv;,x2261
≤,leq,&le;,x2264
≥,geq,&ge;,x2265
⊂,subset,&sub;,x2282
⊃,supset,&sup;,x2283
°,^circ,&deg;,x00B0
×,times,&times;,x00D7
⌊,lfloor,&lfloor;,x230A
⌋,rfloor,&rfloor;,x230B
⌈,lceil,&lceil;,x2308
⌉,rceil,&rceil;,x2309'
  d = read.csv(textConnection(str),header = FALSE,stringsAsFactors = FALSE)

  cat("latex = c(",paste0('"\\\\',d[,2],'"',collapse=","),")")
  uc = d[,4]
  cat("uc = c(",paste0('"\\U',substring(uc,2),'"',collapse=","),")")
}


add.latex.dollar = function(txt, ignore.blocks = list(c("$","$"),c("$$","$$"), c("\\[","\\]"),c("\\(","\\)"), c("`","`"),c("```",c("```")))) {

  setwd("D:/lehre/makro")
  txt = readLines("UmwRes_2.Rmd")

  txt = merge.lines(txt)

  math.chars = "a-zA-Z0-9\\*\\+\\-\\^\\(\\)\\{\\}"
  mp = paste0(" [",math.chars,"]+_[",math.chars,"]+")
  pos = str.find(txt, mp, fixed = FALSE)
  str.at.pos(txt, pos)

  line.blocks = list(c("$$","$$"), c("\\[","\\]"))


}
