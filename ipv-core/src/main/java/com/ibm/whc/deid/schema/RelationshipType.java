/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.schema;

public enum RelationshipType {
  /** Sum relationship type. */
  SUM,
  /** Sum approximate relationship type. */
  SUM_APPROXIMATE,
  /** Product relationship type. */
  PRODUCT,
  /** Equals relationship type. */
  EQUALS,
  /** Greater relationship type. */
  GREATER,
  /** Less relationship type. */
  LESS,
  /** Linked relationship type. */
  LINKED
}
