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

package eu.internetofus.wenet_interaction_protocol_engine;

import eu.internetofus.common.components.Containers;
import eu.internetofus.common.components.incentive_server.WeNetIncentiveServerSimulator;
import eu.internetofus.common.components.personal_context_builder.WeNetPersonalContextBuilderSimulator;
import eu.internetofus.common.components.service.WeNetServiceSimulator;
import eu.internetofus.common.components.social_context_builder.WeNetSocialContextBuilderSimulator;
import eu.internetofus.common.vertx.AbstractMain;
import eu.internetofus.common.vertx.AbstractWeNetComponentIntegrationExtension;
import eu.internetofus.common.vertx.MainArgumentBuilder;
import io.vertx.core.Vertx;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.web.client.WebClientSession;

/**
 * Extension used to run integration tests over the WeNet interaction protocol
 * engine.
 *
 * @author UDT-IA, IIIA-CSIC
 */
public class WeNetInteractionProtocolEngineIntegrationExtension extends AbstractWeNetComponentIntegrationExtension {

  /**
   * {@inheritDoc}
   */
  @Override
  protected String[] createMainStartArguments() {

    final var containers = Containers.status().startBasic().startProfileManagerContainer().startTaskManagerContainer();
    return new MainArgumentBuilder().withApiPort(containers.interactionProtocolEngineApiPort).withComponents(containers)
        .build();

  }

  /**
   * {@inheritDoc}
   *
   * @see Main
   */
  @Override
  protected AbstractMain createMain() {

    return new Main();
  }

  /**
   * {@inheritDoc}
   *
   * @see WeNetServiceSimulator
   * @see WeNetIncentiveServerSimulator
   * @see WeNetSocialContextBuilderSimulator
   * @see WeNetPersonalContextBuilderSimulator
   */
  @Override
  protected void afterStarted(final Vertx vertx, final WebClientSession client, final JsonObject conf) {

    WeNetServiceSimulator.register(vertx, client, conf);
    WeNetIncentiveServerSimulator.register(vertx, client, conf);
    WeNetSocialContextBuilderSimulator.register(vertx, client, conf);
    WeNetPersonalContextBuilderSimulator.register(vertx, client, conf);

  }
}
