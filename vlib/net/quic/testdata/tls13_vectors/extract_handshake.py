import re
import xml.etree.ElementTree as ET

HEX_RE = re.compile(r'^[0-9a-fA-F]*$')


def leaf_hex_concat(elem):
    """Concatenate hex `value=` attributes of every LEAF descendant field, in
    document order, reconstructing the exact raw bytes this field's subtree
    represents (container/expert-info nodes contribute 0 bytes since they
    have no non-empty leaf value of their own).

    Wireshark's dissection tree frequently shows the SAME wire bytes twice
    under sibling leaf fields -- a generic raw field (e.g.
    tls.quic.parameter.value) immediately followed by a friendly, decoded
    alias covering the identical (pos, size) span (e.g.
    tls.quic.parameter.max_idle_timeout). Skip a leaf whose (pos, size)
    exactly matches the immediately-preceding EMITTED leaf's (pos, size) --
    confirmed via this file's own earlier debug walk that such duplicates
    are always adjacent siblings, never separated by other real content."""
    out = []
    last_span = None

    def walk(e):
        nonlocal last_span
        v = e.get('value')
        if v and HEX_RE.match(v):
            # A field with its own non-empty value represents its whole
            # span as one opaque blob -- e.g. tls.handshake.version has
            # value="0303" AND a child (_ws.expert, an annotation, not a
            # byte decomposition). Any children in this case either
            # contribute nothing (like _ws.expert) or would re-decompose
            # the SAME bytes (like the quic.parameter.value/alias
            # duplicates) -- never recurse past a field that already
            # supplied its own value.
            span = (e.get('pos'), e.get('size'))
            if span != last_span:
                out.append(v)
                last_span = span
            return
        for c in e:
            walk(c)

    walk(elem)
    return ''.join(out)


def main():
    tree = ET.parse('tshark_full.pdml')
    root = tree.getroot()

    messages = []
    for pkt_idx, pkt in enumerate(root.findall('packet')):
        for elem in pkt.iter('field'):
            if elem.get('name') != 'tls.handshake':
                continue
            showname = elem.get('showname', '')
            declared_size = int(elem.get('size', '0'))
            raw_hex = leaf_hex_concat(elem)
            actual_size = len(raw_hex) // 2
            messages.append({
                'frame': pkt_idx + 1,
                'showname': showname,
                'declared_size': declared_size,
                'actual_size': actual_size,
                'size_match': declared_size == actual_size,
                'hex': raw_hex,
            })

    for m in messages:
        status = 'OK' if m['size_match'] else 'MISMATCH'
        print(f"frame={m['frame']:2d} {status:8s} declared={m['declared_size']:4d} actual={m['actual_size']:4d}  {m['showname']}")

    with open('handshake_messages.txt', 'w') as f:
        for m in messages:
            f.write(f"# frame={m['frame']} showname={m['showname']} declared_size={m['declared_size']} actual_size={m['actual_size']} size_match={m['size_match']}\n")
            f.write(m['hex'] + '\n\n')


if __name__ == '__main__':
    main()
