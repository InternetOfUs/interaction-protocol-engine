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

import eu.internetofus.common.components.models.Message;
import eu.internetofus.common.components.models.ProtocolNorm;
import eu.internetofus.common.components.models.Task;
import eu.internetofus.common.components.models.TaskType;
import eu.internetofus.common.components.models.WeNetUserProfile;
import eu.internetofus.common.components.service.MessagePredicates;
import eu.internetofus.common.protocols.DefaultProtocols;
import io.vertx.core.Future;
import io.vertx.core.Promise;
import io.vertx.core.Vertx;
import io.vertx.core.json.JsonObject;
import io.vertx.junit5.VertxTestContext;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.function.Predicate;
import org.apache.commons.io.IOUtils;
import org.tinylog.Logger;

/**
 * Test the condition to calculate the normalized socialness.
 *
 * @author UDT-IA, IIIA-CSIC
 */
public abstract class AbstractWhoToAskITC extends AbstractPrologITC {

  /**
   * {@inheritDoc}
   */
  @Override
  protected Future<TaskType> createTaskTypeForProtocol(final Vertx vertx, final VertxTestContext testContext) {

    final Promise<TaskType> promise = Promise.promise();
    DefaultProtocols.ASK_4_HELP_V2.load(vertx).onFailure(promise::fail)
        .onSuccess(ask4HelpType -> super.createTaskTypeForProtocol(vertx, testContext).onFailure(promise::fail)
            .onSuccess(taskType -> {

              taskType.attributes = ask4HelpType.attributes;
              taskType.transactions = ask4HelpType.transactions;
              taskType.callbacks = new JsonObject().put("result",
                  new JsonObject().put("type", "object").put("properties", new JsonObject().put("users",
                      new JsonObject().put("type", "array").put("items", new JsonObject().put("type", "string")))));
              taskType.norms.add(0, new ProtocolNorm());
              taskType.norms.get(0).whenever = "is_received_created_task() and who_to_ask(Users) and length(Users,0)";
              taskType.norms.get(0).thenceforth = "wenet_log_error('No found users to ask',Users)";
              taskType.norms.add(new ProtocolNorm());
              taskType.norms
                  .get(2).whenever = "is_received_do_transaction('moreAnswerTransaction',_) and who_to_ask(Users)";
              taskType.norms.get(
                  2).thenceforth = "add_message_transaction() and send_user_message(\"result\",json([users=Users]))";
              promise.complete(taskType);

            }));

    return promise.future();

  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected String getWheneverCode() {

    return "who_to_ask(Users) and not(length(Users,0))";
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected String getThenceforthCode() {

    return "send_user_message(\"result\",json([users=Users]))";
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected String getOntologyCode() {

    try {

      final var url = this.getClass().getClassLoader()
          .getResource("eu/internetofus/wenet_interaction_protocol_engine/prolog/who_to_ask_test.pl");
      var ontology = IOUtils.toString(url, Charset.defaultCharset());
      ontology = ontology.replaceAll("\\n\\t*", " ").trim();
      Logger.debug("ONTOLOGY CODE is {}", ontology);
      return ontology;

    } catch (final Throwable t) {

      t.printStackTrace();
      return null;
    }

  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected Future<?> doBeforeTaskCreated(final Vertx vertx, final VertxTestContext testContext) {

    return Future.succeededFuture();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected Task createTaskForProtocol() {

    final var task = super.createTaskForProtocol();
    task.attributes = new JsonObject().put("domain", "varia_misc").put("domainInterest", "indifferent")
        .put("beliefsAndValues", "indifferent").put("sensitive", false).put("anonymous", false)
        .put("socialCloseness", "indifferent").put("positionOfAnswerer", "anywhere")
        .put("maxUsers", this.numberOfUsersToCreate());
    return task;

  }

  /**
   * Wait until receive the result message with the specified users.
   *
   * @param vertx       event bus to use.
   * @param testContext context to do the test.
   * @param order       this {@code true} if has to match the same order.
   * @param profiles    of the expected user to be the result.
   *
   * @return the future that check if the result message contains the users.
   */
  protected Future<?> waitUntilResultContainsUsers(final Vertx vertx, final VertxTestContext testContext,
      final boolean order, final WeNetUserProfile... profiles) {

    final var checkMessages = new ArrayList<Predicate<Message>>();
    checkMessages.add(this.createMessagePredicate().and(MessagePredicates.labelIs("result")).and(msg -> {

      final var result = msg.attributes.getJsonArray("users");
      if (result != null) {

        final var max = result.size();
        if (order) {

          if (max == profiles.length) {

            for (var i = 0; i < max; i++) {

              if (!profiles[i].id.equals(result.getString(i))) {

                return false;
              }
            }

            return true;

          }

        } else {

          final var ids = new HashSet<String>();
          for (final var profile : profiles) {

            ids.add(profile.id);
          }
          for (var i = 0; i < max; i++) {

            final var id = result.getString(i);
            if (!ids.remove(id)) {

              return false;
            }

          }

          return ids.isEmpty();
        }

      }
      return false;

    }));

    return this.waitUntilCallbacks(vertx, testContext, checkMessages);
  }

}
