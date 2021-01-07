/*
 * -----------------------------------------------------------------------------
 *
 * Copyright (c) 2019 - 2022 UDT-IA, IIIA-CSIC
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *
 * -----------------------------------------------------------------------------
 */

package eu.internetofus.wenet_interaction_protocol_engine.persistence;

import eu.internetofus.common.components.Model;
import eu.internetofus.common.components.ReflectionModel;
import io.swagger.v3.oas.annotations.media.Schema;

/**
 * A protocol that controls the interaction between users.
 *
 * @author UDT-IA, IIIA-CSIC
 */
public class Protocol extends ReflectionModel implements Model {

  /**
   * The protocol identifier.
   */
  @Schema(description = "The identifier of the protocol.", example = "0")
  public String _id;

  /**
   * The application associated to the protocol.
   */
  @Schema(description = "The identifier of the application associated to the protocol.", example = "0")
  public String appId;

  /**
   * The community associated to the protocol.
   */
  @Schema(description = "The identifier of the community associated to the protocol.", example = "0")
  public String communityId;

  /**
   * The task type associated to the protocol.
   */
  @Schema(description = "The identifier of the task type associated to the protocol.", example = "0")
  public String taskTypeId;

  /**
   * The task associated to the protocol.
   */
  @Schema(description = "The identifier of the task associated to the protocol.", example = "0")
  public String taskId;

  /**
   * The norms for the protocol.
   */
  @Schema(description = "The norms that validated the user interactions.")
  public String norms;

  /**
   * The ontology for the protocol.
   */
  @Schema(description = "The ontology associated to the protocol norms.")
  public String ontology;

}
