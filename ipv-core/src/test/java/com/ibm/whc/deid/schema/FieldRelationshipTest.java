/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.schema;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.ibm.whc.deid.models.ValueClass;
import com.ibm.whc.deid.providers.ProviderType;
import java.io.IOException;
import java.util.Arrays;
import org.junit.Test;

public class FieldRelationshipTest {
  private static final ValueClass VALUE_CLASS = ValueClass.DATE;
  private static final RelationshipType RELATIONSHIP_TYPE = RelationshipType.EQUALS;
  private static final String FIELD_NAME = "FIELD_NAME";

  @Test
  public void testMethods() {
    String relOpName = "REL_OP_NAME";
    ProviderType relOpType = ProviderType.ADDRESS;
    RelationshipOperand relOp = new RelationshipOperand(relOpName, relOpType);
    assertNotNull(relOp.toString());
    final FieldRelationship fieldRel =
        new FieldRelationship(VALUE_CLASS, RELATIONSHIP_TYPE, FIELD_NAME, Arrays.asList(relOp));

    assertEquals("getValueClass", VALUE_CLASS, fieldRel.getValueClass());
    assertEquals("getRelationshipType", RELATIONSHIP_TYPE, fieldRel.getRelationshipType());
    assertEquals("getFieldName", FIELD_NAME, fieldRel.getFieldName());
    RelationshipOperand[] relOps = fieldRel.getOperands();
    assertNotNull("getOperands", relOps);
    assertEquals("getOperands", 1, relOps.length);
    assertEquals("getOperands", relOp, relOps[0]);

    assertNotNull(fieldRel.toString());

    RelationshipOperand relOp2 = new RelationshipOperand(null, relOpType);
    assertNotNull(relOp2.toString());
  }

  @Test
  public void testSerialization() throws IOException {

    String json =
        "{\"fieldName\": \"field1\", \"valueClass\": \"LOCATION\", \"relationshipType\": \"LINKED\", \"operands\": [ {\"name\": \"field2\", \"type\": \"CITY\"} ]}";

    ObjectMapper mapper = new ObjectMapper();
    FieldRelationship fieldRelationship = mapper.readValue(json, FieldRelationship.class);
    assertNotNull(fieldRelationship);
  }
}
