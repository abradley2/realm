open Belt
open HTMLElement
open Event
open Property

let rec patchAttributes = (
  attrs: list<string>,
  toEl: Dom.htmlElement,
  toAttrs: HashMap.String.t<string>,
  fromAttrs: HashMap.String.t<string>,
) =>
  switch attrs {
  | list{} => ()
  | list{attr, ...next} => {
      switch HashMap.String.get(toAttrs, attr) == HashMap.String.get(fromAttrs, attr) {
      | true => ()
      | false =>
        setAttribute(toEl, attr, HashMap.String.get(fromAttrs, attr)->Option.getWithDefault(""))
      }
      patchAttributes(next, toEl, toAttrs, fromAttrs)
    }
  }

let rec deleteAttributes = (attrs: list<string>, toEl: Dom.htmlElement) =>
  switch attrs {
  | list{} => ()
  | list{attr, ...next} => {
      removeAttribute(toEl, attr)
      deleteAttributes(next, toEl)
    }
  }

let rec addAttributes = (
  attrs: list<string>,
  toEl: Dom.htmlElement,
  fromAttrs: HashMap.String.t<string>,
) =>
  switch attrs {
  | list{} => ()
  | list{attr, ...next} => {
      setAttribute(toEl, attr, HashMap.String.get(fromAttrs, attr)->Option.getWithDefault(""))
      addAttributes(next, toEl, fromAttrs)
    }
  }

let rec addEvents = (target, props) => {
  switch props {
  | list{Event(ev), ...next} => ev->applyEvent(target)->Most.merge(addEvents(target, next))
  | list{_prop, ...next} => addEvents(target, next)
  | list{} => Most.empty()
  }
}

let findChildElement = (children: array<HTMLElement.node>, fn: Dom.htmlElement => bool): option<(
  Dom.htmlElement,
  int,
)> => {
  Js.Array2.reducei(
    children,
    (acc: option<(Dom.htmlElement, int)>, cur: HTMLElement.node, idx: int) =>
      switch cur {
      | Element(el) =>
        switch fn(el) {
        | true => Some((el, idx))
        | false => None
        }
      | _ => acc
      },
    None,
  )
}

let render = (toEl: node, streamEl: Element.streamElement<'msg>): Most.stream<'msg> => {
  let fromEl = streamEl.el
  let fromNode = streamEl.vnode

  let streamRef = ref(Most.empty())

  switch (toEl, fromEl) {
  | (Element(toElement), Element(fromElement)) => {
      let toAttrs = getAttributesMap(toElement)
      let fromAttrs = getAttributesMap(fromElement)

      let toAttrNames = toAttrs->HashMap.String.keysToArray->Set.String.fromArray
      let fromAttrNames = fromAttrs->HashMap.String.keysToArray->Set.String.fromArray

      // add attributes that exist on fromEl but not toEl
      Set.String.diff(fromAttrNames, toAttrNames)
      ->Set.String.toList
      ->addAttributes(toElement, fromAttrs)

      // patch attributes that exist on both toEl and fromEl
      Set.String.intersect(fromAttrNames, toAttrNames)
      ->Set.String.toList
      ->patchAttributes(toElement, toAttrs, fromAttrs)

      // remove attributes that exist on toEl, but not on fromEl
      Set.String.diff(toAttrNames, fromAttrNames)->Set.String.toList->deleteAttributes(toElement)

      // we always need to add events
      streamRef.contents = addEvents(toElement, fromNode.properties)->Most.merge(streamRef.contents)

      let toChildren = getChildNodes(toElement)
      let fromChildren = getChildNodes(fromElement)

      fromChildren->Js.Array2.forEachi((fromChild, idx) => {
        // we will usually diff against this child
        let toChildElOpt = toChildren[idx]->Option.flatMap(node => node->mapElement(el => el))

        // if the element has moved, this will be the toEl that had been displaced and what index it currently lives at
        let childMoved = switch fromChild
        ->mapElement(el => getAttribute(el, "data-key"))
        ->Option.flatMap(v => v) {
        | Some(fromKey) =>
          findChildElement(toChildren, el =>
            getAttribute(el, "data-key")
            ->Option.map(toKey => toKey == fromKey)
            ->Option.getWithDefault(false)
          )
        | None => None
        }

        switch childMoved {
        | Some(toChild, toChildIdx) => ()
        | None => ()
        }
      })
    }
  | (Text(toElement), Text(fromElement)) =>
    if getTextContent(toElement) != getTextContent(fromElement) {
      setTextContent(toElement, getTextContent(fromElement))
    }
  | (
      Text(toElement),
      Element(fromElement),
    ) => // if the tag has changed we need an entirely new element
    // if (tag !== domNode.tagName.toLowerCase()) {
    //   const newDomNode = document.createElement(tag)
    //   domNode.parentNode.replaceChild(newDomNode, domNode)
    //   render(virtualNode, newDomNode, activeComponent)
    //   return
    // }
    ()
  | (Element(toElement), Text(fromElement)) => ()
  }

  streamRef.contents
}

/*
function render (virtualNode, domNode, activeComponent) {
  const {
    tagOrComponent,
    props,
    children,
    text
  } = virtualNode

  virtualNode.domNode = domNode

  // let's handle components!
  if (tagOrComponent.constructor === Function) {
    // check if we're at a new root component for our render tree
    if (tagOrComponent !== (hooks.activeComponentNode() && hooks.activeComponentNode().tagOrComponent)) {
      hooks.hooksCache({})
      hooks.activeComponentNode(virtualNode)
    }

    const componentNode = tagOrComponent(props, virtualNode)
    if (!domNode) {
      // we don't have a dom node and it's a component, we need to render and
      // return the vdom of the component to the parent cycle of this render
      // function so it can mount it
      return componentNode
    }
    render(componentNode, domNode, tagOrComponent)
    return
  }

  const tag = tagOrComponent

  // it's a text node! these are easy to handle
  if (!domNode.tagName) {
    if (text !== domNode.textContent) {
      domNode.textContent = text
    }
    return
  }

  // if the tag has changed we need an entirely new element
  if (tag !== domNode.tagName.toLowerCase()) {
    const newDomNode = document.createElement(tag)
    domNode.parentNode.replaceChild(newDomNode, domNode)
    render(virtualNode, newDomNode, activeComponent)
    return
  }

  // now we need to diff/set props to attributes. This is pretty easy
  Object.keys(props).forEach((propName) => {
    const propValue = props[propName]

    // skip children
    if (propName === 'children') {
      return
    }

    // add event handlers directly to the domNode
    if (typeof propValue === 'function') {
      if (!eventHandlers.includes(propName)) {
        eventHandlers.push(propName)
      }
      domNode[propName] = propValue
      return
    }

    // handle id and className as special cases, everything else
    // is just set attribute from here
    switch (domProperties.includes(propName)) {
      case true:
        domNode[propName] = propValue
        break
      default:
        domNode.setAttribute(propName, propValue)
    }
  })

  // if there are attributes in the domNode that are no longer in props
  // those need to be unset
  if (domNode.hasAttributes()) {
    for (let i = 0; i < domNode.attributes.length; i++) {
      const { name: attrName, value: attrValue } = domNode.attributes[i]

      // class- a pain in the ass
      if (attrName === 'class') {
        if (!props.className && domNode.className) {
          domNode.className = ''
          continue
        }
      }

      if (domProperties.includes(attrName)) {
        if (!props[attrName] && domNode[attrName]) {
          domNode[attrName] = undefined
          continue
        }
      }

      if (typeof props[attrName] === 'undefined' && attrValue) {
        domNode.removeAttribute(attrName)
      }
    }
  }

  // handle event handlers
  eventHandlers.forEach((handlerName) => {
    if (!props[handlerName] && domNode[handlerName]) {
      domNode[handlerName] = undefined
    }
  })

  // add and edit children as needed
  children.forEach((virtualChild, idx) => {
    let domSibling = domNode.childNodes[idx]
    const nextSibling = domNode.childNodes[idx + 1]

    if (!virtualChild) {
      return
    }

    // first let's check if children has been "re-arranged"
    // and if we can use the "key" optimization
    let childMoved = false
    if (virtualChild.props.key) {
      // check if there's a key match
      domSibling = [...domNode.childNodes].find((node, searchIdx) => {
        const match = node.getAttribute('key') === virtualChild.props.key
        if (match && idx !== searchIdx) {
          childMoved = true
        }
        return match
      }) || domSibling
    }

    // handle child move using cached keyed dom node
    if (childMoved) {
      domNode.removeChild(domSibling)
      // we need to re-compute this as the previous nextSibling
      // could be the key-match which we have just removed
      const newNextSibling = domNode.childNodes[idx + 1]
      if (newNextSibling) {
        domNode.insertBefore(domSibling, nextSibling)
      } else {
        domNode.appendChild(domSibling)
      }
    }

    // handle no matching host node
    if (!domSibling && virtualChild.tagOrComponent.constructor === String) {
      domSibling = virtualChild.tagOrComponent === 'TEXT_NODE'
        ? document.createTextNode(virtualChild.children[0])
        : document.createElement(virtualChild.tagOrComponent)
      if (nextSibling) {
        domNode.insertBefore(domSibling, nextSibling)
      } else {
        domNode.appendChild(domSibling)
      }
    }

    // now that it is guaranteed to have found a real dom node sibling match
    // each virtual child node can go through it's own render
    let result = render(virtualChild, domSibling)

    while (result) {
      if (result.tagOrComponent.constructor === String) {
        const hostEl = document.createElement(result.tagOrComponent)
        hostEl.__hooksCache = hooks.hooksCache()

        Object.assign(
          hooks.activeComponentNode(),
          { domNode: hostEl }
        )

        domNode.appendChild(hostEl)
        render(result, hostEl, activeComponent)
        result = undefined
        continue
      }

      // we're at a new component in the heirarchy so we need to reset active component
      hooks.activeComponentNode(undefined)
      hooks.hooksCache({})
      result = render(result, domSibling, activeComponent)
    }
  })

  hooks.activeComponentNode(undefined)
  hooks.hooksCache({})
}
*/
