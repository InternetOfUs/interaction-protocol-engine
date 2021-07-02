/*
 * -----------------------------------------------------------------------------
 *
 * Copyright 2019 - 2022 UDT-IA, IIIA-CSIC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * -----------------------------------------------------------------------------
 */

package eu.internetofus.wenet_interaction_protocol_engine.api;

import org.junit.jupiter.api.extension.ExtendWith;

import eu.internetofus.common.vertx.AbstractAPIVerticleIntegrationTestCase;
import eu.internetofus.wenet_interaction_protocol_engine.WeNetInteractionProtocolEngineIntegrationExtension;
import eu.internetofus.wenet_interaction_protocol_engine.api.norms.Norms;
import io.vertx.core.json.JsonObject;

/**
 * Integration tests of the {@link APIVerticle}.
 *
 * @author UDT-IA, IIIA-CSIC
 */
@ExtendWith(WeNetInteractionProtocolEngineIntegrationExtension.class)
public class APIVerticleIT extends AbstractAPIVerticleIntegrationTestCase {

  /**
   * {@inheritDoc}
   */
  @Override
  protected String getBadRequestPostPath() {

    return Norms.PATH;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected JsonObject createBadRequestPostBody() {

    return new JsonObject().put("id", new JsonObject().put("key", "value"));
  }
}
