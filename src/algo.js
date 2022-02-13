function patchChildren (domNode, vNode) {
  vNode.children.forEach((vNodeChild, idx) => {
    let childMoved = null
    
    // the sibling that aligns to the virtual node child by index
    // this will sometimes be "null" when a child is appended in the virtual-dom,
    // causing there to be excess nodes in the vdom compared to the dom
    let domNodeChild = domNode.childNodes[idx]

    // first check if we have a key attribute
    if (vNode.getAttribute('data-key')) {
      childMoved = [...domNode.childNodes].find((child, childIdx) => {
        // if we find another node with the matching key attribute, AND at a different index
        // then we know this child has moved
        return childIdx !== idx && child.getAttribute('data-key') === vNode.getAttribute('data-key')
          
      }) || null
    }

    // handle cases with a moved node
    if (childMoved) {
      // if there's a "sibling" to this virtual node on the real dom, we can 
      // "insertBefore" such that the moved vNode is now correctly at it's sibling's index
      if (domNodeChild) {
        domNode.insertBefore(vNodeChild, domNodeChild)
        return
      }
      // if there's not a sibling, that means we need to append 
      domNode.appendChild(vNodeChild)
      return
    }

    // if the node hasn't moved, we only need to check if we need to append a new node
    if (!domNodeChild) {
      domNode.appendChild(vNodeChild)
    }
  })

  // now remove all excess nodes that exist on the real dom but don't exist on the vdom
  while (domNode.childNodes.length > vNode.children.length) {
    domNode.removeChild(domNode.lastChild)
  }

  // now we dom and the vdom are ligned up in terms of children and we can diff/patch their children
}
