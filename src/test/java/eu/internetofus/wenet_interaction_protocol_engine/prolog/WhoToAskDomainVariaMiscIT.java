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
package eu.internetofus.wenet_interaction_protocol_engine.prolog;

import eu.internetofus.common.components.models.Task;
import eu.internetofus.common.components.models.WeNetUserProfile;
import io.vertx.core.Future;
import io.vertx.core.Vertx;
import io.vertx.junit5.VertxTestContext;

/**
 * Test the condition to calculate the who to ask with the domain of type
 * {@code varia-misc}.
 *
 * @author UDT-IA, IIIA-CSIC
 */
public class WhoToAskDomainVariaMiscIT extends AbstractWhoToAskITC {

  /**
   * {@inheritDoc}
   */
  @Override
  protected Task createTaskForProtocol() {

    final var task = super.createTaskForProtocol();
    task.goal.name = "Witch is the best nationality flag?";
    task.attributes.put("domain", "varia_misc").put("domainInterest", "similar");
    return task;

  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected Future<?> doAfterTaskCreated(final Vertx vertx, final VertxTestContext testContext) {

    return this
        .waitUntilResultContainsUsers(vertx, testContext, false,
            this.users.subList(1, this.users.size()).toArray(new WeNetUserProfile[this.users.size() - 1]))
        .compose(ignored -> this.waitUntilUserTaskState(this.users.get(0).id, vertx, testContext, userTaskState -> {
          final var unaskedUserIds = userTaskState.attributes.getJsonArray("unaskedUserIds");
          return unaskedUserIds != null && unaskedUserIds.isEmpty();
        }));

  }

}
