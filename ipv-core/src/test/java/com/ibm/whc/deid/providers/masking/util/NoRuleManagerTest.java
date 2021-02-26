/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking.util;

import static org.junit.Assert.assertEquals;
import java.util.ArrayList;
import java.util.List;
import org.junit.Test;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.TextNode;
import com.ibm.whc.deid.ObjectMapperFactory;
import com.ibm.whc.deid.providers.masking.NullMaskingProvider;
import com.ibm.whc.deid.providers.masking.fhir.MaskingActionInputIdentifier;
import com.ibm.whc.deid.providers.masking.fhir.MaskingProviderBuilder;

public class NoRuleManagerTest {

  @Test
  public void testMain() throws Exception {
    NullMaskingProvider provider = new NullMaskingProvider();
    ObjectMapper om = ObjectMapperFactory.getObjectMapper();

    StringBuilder buffer = new StringBuilder(100);
    buffer.append("{");
    buffer.append("\"resourceType\": \"Organization\",");
    buffer.append("\"id\": \"4\",");
    buffer.append("\"address\": [");
    buffer.append("    {\"line\": [");
    buffer.append("       \"v101\",");
    buffer.append("       \"v102\",");
    buffer.append("       \"v101\",");
    buffer.append("       \"v103\",");
    buffer.append("       null,");
    buffer.append("       {\"a\":1, \"b\":2},");
    buffer.append("       [\"box1\", \"box2\"],");
    buffer.append("       [{\"c\":3, \"d\":4, \"e\":null}]");
    buffer.append("] } ] }");
    String doc = buffer.toString();

    JsonNode root = om.readTree(doc);
    NoRuleManager mgr = new NoRuleManager(
        new MaskingProviderBuilder.MaskingResource("id", root, null), "r1", provider);
    mgr.applyToRemainingNodes();
    assertEquals(
        "{\"resourceType\":null,\"id\":null,\"address\":[{\"line\":[null,null,null,null,null,{\"a\":null,\"b\":null},[null,null],[{\"c\":null,\"d\":null,\"e\":null}]]}]}",
        om.writeValueAsString(root));

    root = om.readTree(doc);
    mgr = new NoRuleManager(new MaskingProviderBuilder.MaskingResource("id", root, null), "r1",
        provider);
    JsonNode branchNode = root.get("address");
    JsonNode otherNode = om.readTree("{\"a\":1}");
    JsonNode lineNode = root.get("address").get(0).get("line");
    JsonNode childNode = lineNode.get(2);
    JsonNode cParentNode = root.get("address").get(0).get("line").get(7).get(0);
    JsonNode cNode = cParentNode.get("c");
    JsonNode eNode = cParentNode.get("e");
    List<MaskingActionInputIdentifier> list = new ArrayList<>();
    // not leaf, ignored
    list.add(new MaskingActionInputIdentifier(null, branchNode, root, "address", "Organization",
        "r2", root));
    // created by masking actions, not an original node
    list.add(new MaskingActionInputIdentifier(null, otherNode, root, "address", "Organization",
        "r2", root));
    // original array node
    list.add(new MaskingActionInputIdentifier(null, childNode, lineNode, "line[2]", "Organization",
        "r2", root));
    // original node
    list.add(new MaskingActionInputIdentifier(null, cNode, cParentNode, "c", "Organization", "r2",
        root));
    // already null node
    list.add(new MaskingActionInputIdentifier(null, eNode, cParentNode, "e", "Organization", "r2",
        root));
    mgr.removeNodesAlreadyMasked(list);
    mgr.applyToRemainingNodes();
    assertEquals(
        "{\"resourceType\":null,\"id\":null,\"address\":[{\"line\":[null,null,\"v101\",null,null,{\"a\":null,\"b\":null},[null,null],[{\"c\":3,\"d\":null,\"e\":null}]]}]}",
        om.writeValueAsString(root));

    // root is value node - not supported by system
    JsonNode valueRoot = new TextNode("lion");
    mgr = new NoRuleManager(new MaskingProviderBuilder.MaskingResource("id", valueRoot, null), "r3",
        provider);
    list.clear();
    list.add(new MaskingActionInputIdentifier(null, valueRoot, valueRoot, "", "", "r3", valueRoot));
    mgr.removeNodesAlreadyMasked(list);
    mgr.applyToRemainingNodes();
    assertEquals("\"lion\"", om.writeValueAsString(valueRoot));
  }
}
