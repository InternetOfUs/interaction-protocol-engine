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

import eu.internetofus.common.components.Containers;
import eu.internetofus.common.components.models.Message;
import eu.internetofus.common.components.models.ProtocolNorm;
import eu.internetofus.common.components.models.SocialNetworkRelationship;
import eu.internetofus.common.components.models.SocialNetworkRelationshipType;
import eu.internetofus.common.components.models.Task;
import eu.internetofus.common.components.models.TaskTransaction;
import eu.internetofus.common.components.models.TaskType;
import eu.internetofus.common.components.models.WeNetUserProfile;
import eu.internetofus.common.components.profile_manager.WeNetProfileManager;
import eu.internetofus.common.components.service.MessagePredicates;
import eu.internetofus.common.components.task_manager.TaskPredicates;
import eu.internetofus.common.components.task_manager.TaskTransactionPredicates;
import eu.internetofus.common.components.task_manager.WeNetTaskManager;
import eu.internetofus.common.protocols.DefaultProtocols;
import eu.internetofus.wenet_interaction_protocol_engine.persistence.StatesRepositoryImpl;
import io.vertx.core.Future;
import io.vertx.core.Promise;
import io.vertx.core.Vertx;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.mongo.MongoClient;
import io.vertx.junit5.Timeout;
import io.vertx.junit5.VertxTestContext;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.concurrent.TimeUnit;
import java.util.function.Predicate;
import org.apache.commons.io.IOUtils;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;

/**
 * Test the condition to calculate the normalized socialness.
 *
 * @author UDT-IA, IIIA-CSIC
 */
public class WhoToAskIT extends AbstractPrologITC {

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

    return "who_to_ask(Users)";
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
      System.out.println(ontology);
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
  protected Future<?> waitUntilResultcontainsUsers(final Vertx vertx, final VertxTestContext testContext,
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

  /**
   * {@inheritDoc}
   */
  @Override
  protected Future<?> doAfterTaskCreated(final Vertx vertx, final VertxTestContext testContext) {

    return this.waitUntilResultcontainsUsers(vertx, testContext, false,
        this.users.subList(1, this.users.size()).toArray(new WeNetUserProfile[this.users.size() - 1]));

  }

  /**
   * Check that can ask to the different social closeness.
   *
   * @param vertx       event bus to use.
   * @param testContext context to do the test.
   */
  @Test
  @Order(6)
  @Timeout(value = 1, timeUnit = TimeUnit.MINUTES)
  public void shouldAskUsersByDifferentSocialCloseness(final Vertx vertx, final VertxTestContext testContext) {

    this.assertLastSuccessfulTestWas(5, testContext);

    final var newTask = new Task();
    newTask.attributes = new JsonObject().put("socialCloseness", "different").put("maxUsers", 2);

    final var profile = this.users.get(0);
    profile.relationships = new ArrayList<>();
    for (var i = 1; i < this.users.size() - 1; i++) {

      final var relationship = new SocialNetworkRelationship();
      relationship.appId = this.app.appId;
      relationship.type = SocialNetworkRelationshipType.values()[i % SocialNetworkRelationshipType.values().length];
      relationship.userId = this.users.get(i).id;
      relationship.weight = 1.0 - 0.1 * (i + 1);
      profile.relationships.add(relationship);

    }

    final var moreAnswerTransaction = new TaskTransaction();
    moreAnswerTransaction.taskId = this.task.id;
    moreAnswerTransaction.actioneerId = this.task.requesterId;
    moreAnswerTransaction.label = "moreAnswerTransaction";
    final var checkTask = this.createTaskPredicate()
        .and(TaskPredicates.lastTransactionIs(
            this.createTaskTransactionPredicate().and(TaskTransactionPredicates.similarTo(moreAnswerTransaction))
                .and(TaskTransactionPredicates.messagesSizeIs(1))));

    MongoClient.create(vertx, Containers.status().getMongoDBConfig())
        .dropCollection(StatesRepositoryImpl.STATES_COLLECTION)
        .compose(ignored -> WeNetTaskManager.createProxy(vertx).mergeTask(this.task.id, newTask))
        .compose(ignored -> WeNetProfileManager.createProxy(vertx).updateProfile(profile).map(updated -> {
          this.users.remove(0);
          this.users.add(0, updated);
          return null;
        })).compose(ignored -> WeNetTaskManager.createProxy(vertx).doTaskTransaction(moreAnswerTransaction))
        .compose(ignored -> this.waitUntilTask(vertx, testContext, checkTask))
        .compose(ignored -> this.waitUntilUserTaskState(this.task.requesterId, vertx, testContext, userTaskState -> {

          if (userTaskState.attributes != null) {

            final var socialClosenessUsers = userTaskState.attributes.getJsonArray("socialClosenessUsers");
            for (var i = this.users.size() - 1; i > 0; i--) {

              final var userId = this.users.get(i).id;
              final var value = 100 - (9 - i) * 10;
              var found = false;
              for (var j = 0; j < socialClosenessUsers.size(); j++) {

                final var element = socialClosenessUsers.getJsonObject(j);
                if (userId.equals(element.getString("userId"))) {

                  found = Math.round(element.getDouble("value") * 100) == value;
                  break;
                }
              }
              if (!found) {

                return false;
              }
            }

            final var whoToAsk = userTaskState.attributes.getJsonArray("whoToAskUsers");
            var j = 0;
            for (var i = this.users.size() - 1; i > 0; i--, j++) {

              final var userId = this.users.get(i).id;
              final var value = 100 - (9 - i) * 10;
              final var element = whoToAsk.getJsonObject(j);
              if (!userId.equals(element.getString("userId"))
                  && Math.round(element.getDouble("value") * 100) != value) {

                return false;
              }
            }

            final var unaskedUserIds = userTaskState.attributes.getJsonArray("unaskedUserIds");
            j = 0;
            for (var i = this.users.size() - 3; i > 0; i--, j++) {

              final var userId = this.users.get(i).id;
              if (!userId.equals(unaskedUserIds.getString(j))) {

                return false;
              }
            }

            return true;
          }

          return false;

        }))
        .compose(ignored -> this.waitUntilResultcontainsUsers(vertx, testContext, true, this.users.get(9),
            this.users.get(8)))
        .compose(ignored -> WeNetTaskManager.createProxy(vertx).doTaskTransaction(moreAnswerTransaction))
        .compose(ignored -> this.waitUntilTask(vertx, testContext, checkTask.and(TaskPredicates.transactionSizeIs(3))))
        .compose(ignored -> this.waitUntilResultcontainsUsers(vertx, testContext, true, this.users.get(7),
            this.users.get(6)))
        .compose(ignored -> WeNetTaskManager.createProxy(vertx).doTaskTransaction(moreAnswerTransaction))
        .compose(ignored -> this.waitUntilTask(vertx, testContext, checkTask.and(TaskPredicates.transactionSizeIs(4))))
        .compose(ignored -> this.waitUntilResultcontainsUsers(vertx, testContext, true, this.users.get(5),
            this.users.get(4)))
        .compose(ignored -> WeNetTaskManager.createProxy(vertx).doTaskTransaction(moreAnswerTransaction))
        .compose(ignored -> this.waitUntilTask(vertx, testContext, checkTask.and(TaskPredicates.transactionSizeIs(5))))
        .compose(ignored -> this.waitUntilResultcontainsUsers(vertx, testContext, true, this.users.get(3),
            this.users.get(2)))
        .compose(ignored -> WeNetTaskManager.createProxy(vertx).doTaskTransaction(moreAnswerTransaction))
        .compose(ignored -> this.waitUntilTask(vertx, testContext, checkTask.and(TaskPredicates.transactionSizeIs(6))))
        .compose(ignored -> this.waitUntilResultcontainsUsers(vertx, testContext, true, this.users.get(1)))
        .compose(ignored -> WeNetTaskManager.createProxy(vertx).doTaskTransaction(moreAnswerTransaction))
        .compose(ignored -> this.waitUntilTask(vertx, testContext, checkTask.and(TaskPredicates.transactionSizeIs(7))))
        .compose(ignored -> this.waitUntilResultcontainsUsers(vertx, testContext, true, new WeNetUserProfile[0]))
        .onComplete(testContext.succeeding(ignored -> this.assertSuccessfulCompleted(testContext)));

  }

}
