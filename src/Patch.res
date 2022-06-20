open Belt
open HTMLElement
open Event
open Property
open VirtualElement
open Most

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
  | list{attr, ...next} =>
    if Js.String.includes("realm-root-element--", attr) {
      deleteAttributes(next, toEl)
    } else {
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

let rec findChildElement = (children: list<HTMLElement.node>, fn: Dom.htmlElement => bool): option<
  Dom.htmlElement,
> =>
  switch children {
  | list{} => None
  | list{Element(child), ...next} =>
    switch fn(child) {
    | true => Some(child)
    | false => findChildElement(next, fn)
    }
  | list{Text(_text), ...next} => findChildElement(next, fn)
  }

let rec render = (
  toEl: node,
  virtualElement: virtualElement<'msg>,
  streamRef: ref<stream<'msg>>,
): stream<'msg> => {
  let fromEl = virtualElement.el
  let fromNode = virtualElement.vnode

  switch (toEl, fromEl) {
  | (Element(toElement), Element(fromElement)) => {
      if tagName(toElement) != tagName(fromElement) {
        let toNode = liftElement(toElement)
        let fromNode = liftElement(fromElement)
        getParent(toNode)->replaceChild(fromNode, toNode)
        let _ = render(node(fromNode), virtualElement, streamRef)
      }

      let toAttrs = getAttributesMap(toElement)
      let fromAttrs = Property.toAttributesMap(virtualElement.vnode.properties)

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
      let toParent = toElement

      virtualElement.children->List.forEachWithIndex((idx, fromVChild) => {
        // we will usually diff against this child
        let toSibling = toElement->getChildNodeAt(idx)

        // if the element has moved, this will be the toEl that had been displaced and what index it currently lives at
        let childMoved = switch fromVChild.el
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
        | Some(childMoved) => {
            removeChild(toParent, childMoved->liftElement)
            // if the moved element has an existing sibling, we can insertBefore on that to put it in the correct place
            switch toSibling {
            | Some(toSibling) => insertBefore(childMoved->liftElement, toSibling->toDomNode)
            // otherwise that means the we're at the end of the child list and we need to append
            | None => appendChild(toParent, childMoved->liftElement)
            }
          }

        | None => {
            switch toSibling {
            | None => fromVChild.el->toDomNode->cloneNode(true)->appendChild(toParent, _)
            // otherwise we have a match, no additional work is needed, we can go straight to diff/patch
            | Some(_) => ()
            }
            ()
          }
        }

        let targetChild = switch childMoved {
        | Some(child) => Element(child)
        | None => toParent->getChildNodeAt(idx)->Option.getExn
        }

        // TODO: need to make this call tail-recursive
        render(targetChild, fromVChild, streamRef)
      })

      // remove any children that are no longer in the virtual tree
      while List.length(virtualElement.children) < List.length(getChildNodes(toElement)) {
        switch lastChild(toElement) {
        | Some(child) => removeChild(toElement, child)
        | None => ()
        }
      }
    }
  | (Text(toElement), Text(fromElement)) =>
    if getTextContent(toElement) != getTextContent(fromElement) {
      setTextContent(toElement, getTextContent(fromElement))
    }
  | (Text(toElement), Element(fromElement)) => {
      let toNode = liftText(toElement)
      let fromNode = liftElement(fromElement)
      getParent(toNode)->replaceChild(fromNode, toNode)
      let _ = render(node(fromNode), virtualElement, streamRef)
    }
  | (Element(toElement), Text(fromElement)) => {
      let toNode = liftElement(toElement)
      let fromNode = liftText(fromElement)
      getParent(toNode)->replaceChild(fromNode, toNode)
      let _ = render(node(fromNode), virtualElement, streamRef)
    }
  }

  streamRef.contents
}
