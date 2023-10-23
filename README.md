# WizLang
Silly language to build a lexer, parser and compiler for

## Spec
$$
\begin{align}
  [\text{prog}] &\to [\text{stmt}]^+ \\
  [\text{stmt}] &\to 
  \begin{cases} 
    declare \space \text{ident} \space as \space [\text
    {expr}]. \\
    imbue \space \text{ident} \space as \space [\text{expr}]. \\
    conjure \space \text{ident}, \space which \space takes \space [[\text{ident}]\space as \space [\text{type}]]^*, \space \\ \space thusly \space containing, \space [stmt]^* \space conclude. \\
    return \space [\text{expr}]. \\ 
    should \space [\text{expr}], \space then \space [\text{stmt}]*
  \end{cases} \\
  [\text{expr}] \space &\to
  \begin{cases}
    [\text{lit}] \\
    \text{ident} \\
    [\text{expr}]'s \space \text{ident} \\
    [\text{expr}] \space [\text{op}] \space [\text{expr}] \\
    invoke \space [\text{expr}], \space which \space takes \space [\text{expr}]^*
  \end{cases} \\
  [\text{lit}] \space &\to 
  \begin{cases}
    \text{integer} \\
    \text{float} \\ 
    \text{string} \\ 
    \text{boolean} \\
  \end{cases} \\
  [\text{type}] \space &\to 
  \begin{cases}
    \text{lit} \\ 
    \text{custom}
  \end{cases} \\
  [\text{op}] \space &\to 
  \begin{cases}
    be \space greater \space than: &> \\
    be \space greater \space than \space or \space equal \space to \space: &\geq \\
    be \space less \space than: &< \\
    be \space less \space than \space or \space equal \space to: &\leq \\
    be \space equal \space to: &= \\
    be \space unequal \space to: & \neq \\
    and: & \&\& \\
    or: & ||
  \end{cases}
\end{align}
$$