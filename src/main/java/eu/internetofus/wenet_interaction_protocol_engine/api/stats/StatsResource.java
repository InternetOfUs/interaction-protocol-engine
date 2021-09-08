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
package eu.internetofus.wenet_interaction_protocol_engine.api.stats;

import io.vertx.core.AsyncResult;
import io.vertx.core.Handler;
import io.vertx.core.Vertx;
import io.vertx.ext.web.api.service.ServiceRequest;
import io.vertx.ext.web.api.service.ServiceResponse;

/**
 * Resource to provide the stats about the user interactions.
 *
 * @see Stats
 *
 * @author UDT-IA, IIIA-CSIC
 */
public class StatsResource implements Stats {

  /**
   * The event bus that is using.
   */
  protected Vertx vertx;

  /**
   * Create a new instance to provide the services of the {@link Stats}.
   *
   * @param vertx with the event bus to use.
   */
  public StatsResource(final Vertx vertx) {

    this.vertx = vertx;

  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void countInteractions(final String sourceId, final String targetId, final Long from, final Long to,
      final ServiceRequest context, final Handler<AsyncResult<ServiceResponse>> resultHandler) {
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void getInteractionBetweenUsersPage(final String sourceId, final String targetId, final Long from,
      final Long to, final int offset, final int limit, final ServiceRequest context,
      final Handler<AsyncResult<ServiceResponse>> resultHandler) {
  }

}
